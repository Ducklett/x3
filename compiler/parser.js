const path = require("path")
const { assert, read, spanFromRange } = require('./util')
const { lex, preUnaryOperators, postUnaryOperators, binaryOperators, assignmentOperators } = require('./lexer')
const { error, reportError } = require("./errors")

const fileMap = new Map()

function parse(source) {
	fileMap.set(source.path, source.code)
	const code = source.code

	const tokens = lex(code, source.path)
	const statements = _parse(tokens)
	return statements

	function _parse(tokens) {
		let isTopLevel = true
		const filesToImport = new Set()
		// let lastSymbol = null
		tokens = tokens.filter((t) => t.kind !== 'whitespace' && t.kind !== 'comment')
		let tokenIndex = 0

		let contents = parseFile()
		const files = [contents]
		for (let filePath of filesToImport) {
			const goesBack = filePath.value.startsWith('../')
			assert(filePath.value.startsWith('./') || goesBack, `import file path always starts with ./ or ../`)
			let p = goesBack ? filePath.value : filePath.value.slice(2)
			if (!p.endsWith('.x3')) p += '.x3'

			const sourcePath = path.join(path.parse(source.path).dir, p)
			if (fileMap.has(sourcePath)) {
				console.log('NOTE: file already imported! skipping')
				continue
			}
			const importedSource = { path: sourcePath, code: read(sourcePath) }
			const parsedSource = parse(importedSource, fileMap)
			for (const file of parsedSource) {
				files.push(file)
			}
		}
		return files

		function peek(n) { return tokens[tokenIndex + n] }
		function current() {
			return peek(0)
		}

		function isEndOfFile() {
			return !current()
		}

		function is(kind, value = null) {
			const got = current()
			if (!got) return false
			return got.kind == kind && (value === null || got.value === value)
		}
		function currentIsPreUnaryOperator() {
			const cur = current()
			if (!cur) return false
			return cur.kind == 'operator' && preUnaryOperators.has(cur.value)
		}
		function currentIsPostUnaryOperator() {
			const cur = current()
			if (!cur) return false
			return cur?.kind == 'operator' && postUnaryOperators.has(cur.value)
		}
		function currentIsBinaryOperator() {
			const cur = current()
			if (!cur) return false
			return cur.kind == 'operator' && binaryOperators.has(cur.value)
		}
		function isAssignmentOperator(it = null) {
			if (!it) it = current()
			return it.kind == 'operator' && assignmentOperators.has(it.value)
		}
		function expect(kind, value = null) {
			const got = current()
			if (got.kind != kind) {
				const at = `${source.path}:${got.span.from}`
				throw (`${at}: expected token of kind ${kind}, got ${got.kind}::${got.value}`)
			}

			if (value !== null && got.value != value) {
				const at = `${source.path}:${got.span.from}`
				throw (`${at}: expected token with value ${value}, got ${got.value}`)
			}
		}

		function save() { return tokenIndex }
		function restore(i) {
			assert(i > 0 && i < tokens.length)
			tokenIndex = i
		}

		function take(kind, value) {
			if (kind) expect(kind, value)
			return tokens[tokenIndex++]
		}

		function parseFile() {
			const declarations = []
			while (true) {
				if (!current()) break
				const cur = current()
				const stmt = parseDeclaration('file')
				if (!stmt) {
					reportError(error.expectedStatement(take()))
				}
				declarations.push(stmt)
			}

			assert(!current(), "all tokens consumed at end of file")
			return { kind: 'file', declarations }
		}

		function parseStringLiteral() { return take('string') }

		function parseAssignment(symbol) {
			if (!symbol) symbol = parseSymbol()
			const operator = take('operator')
			const expr = parseExpression()
			const span = spanFromRange(symbol.span, expr.span)
			return { kind: 'assignment', name: symbol, operator, expr, span }
		}

		function parseExpression(noBinary = false) {
			let lhs
			if (currentIsPreUnaryOperator()) {
				const op = take('operator')
				const expr = parsePrimaryExpression()
				const span = spanFromRange(op.span, expr.span)
				lhs = { kind: 'pre unary', expr, op, span }
			} else {
				const expr = parsePrimaryExpression()
				if (currentIsPostUnaryOperator()) {
					const op = take('operator')
					const span = spanFromRange(expr.span, op.span)
					lhs = { kind: 'post unary', expr, op, span }
				} else {
					lhs = expr
				}
			}

			if (is('operator', ':')) {
				const colon = take('operator', ':')
				const type = parseType()
				lhs = { kind: 'cast', expr: lhs, colon, type, span: spanFromRange(lhs.span, type.span) }
			}

			if (is('operator', '~')) {
				const colon = take('operator', '~')
				const type = parseType()
				lhs = { kind: 'reinterpret', expr: lhs, colon, type, span: spanFromRange(lhs.span, type.span) }
			}

			// HACK: this is needed to make casts and unary work on rhs until we implement precedence
			if (noBinary) return lhs

			while (currentIsBinaryOperator()) {
				const op = take('operator')
				// TODO: precedence climbing
				// NOTE: only the first part of parseExpression can currently have unary operator
				const rhs = parseExpression(true)
				assert(rhs)
				const span = spanFromRange(lhs.span, rhs.span)
				lhs = { kind: 'binary', lhs, op, rhs, span }
			}
			return lhs

			function parseSymbolExpression() {
				let lhs = parseSymbol()

				// TODO: find a nicer way to do this..
				while (true) {
					if (is('operator', '.')) {
						// property acces
						const dot = take('operator', '.')
						const property = parseSymbol()
						const span = spanFromRange(lhs.span, property.span)

						// use property access as lhs, try for assignement
						lhs = { kind: 'property access', scope: lhs, dot, property, span }
						continue
					}

					if (is('operator', '(')) {

						if (lhs.value == 'sizeof') {
							const argumentList = parseList(parseType)
							const span = spanFromRange(lhs.span, argumentList.span)
							lhs = { kind: 'call', name: lhs, argumentList, span }
							break
						}

						// function call
						{

							const argumentList = parseList(parseExpression)
							const span = spanFromRange(lhs.span, argumentList.span)
							lhs = { kind: 'call', name: lhs, argumentList, span }
							continue
						}
					}

					if (is('operator', '[')) {
						const begin = take('operator', '[')
						const index = parseExpression()
						const end = take('operator', ']')
						const span = spanFromRange(lhs.span, end.span)
						lhs = { kind: 'offset access', name: lhs, begin, index, end, span }
						continue
					}

					if (isAssignmentOperator(current())) return parseAssignment(lhs)

					break
				}

				return lhs
			}

			function parseParenthesizedExpressionOrLambda() {
				// we can figure out which it is in several cases:
				//    v no arg; lambda
				// - () {}
				//           NOTE: this means you can't have a parenthesized binding expression followed by a block literal
				//           TODO: special case in condition statements so if '(x:int) {}' parses correctly
				//           v block; lambda
				// - (x:int) {}
				//           v arrow,lambda
				// - (x:int) -> void {}
				//         v comma,lambda
				// - (x:int,) {}
				//           v tag,lambda
				// - (x:int) #foo {}

				let isLambda = false
				const snapshot = save()

				take('operator', '(')

				if (is('operator', ')')) {
					isLambda = true
				} else {
					if (is('symbol')) {
						parseSymbol()
						if (is('operator', ':')) {
							take('operator', ':')
							parseType()
							if (is('operator', ',')) {
								isLambda = true
							} else if (is('operator', ')')) {
								take('operator', ')')
								if (is('operator', '#') || is('operator', '->') || is('operator', '{')) {
									isLambda = true
								}
							}
						}
					}
				}

				// TODO: rewrite parseSymbol so it doesn't mutate anything
				// symbol mutations will fuck up the parser, 
				restore(snapshot)

				if (isLambda) {
					let parameters = parseList(parseTypedSymbol)
					let arrow, returnType
					if (is('operator', '->')) {
						arrow = take('operator', '->')
						returnType = parseType()
					}
					const tags = parseTags()
					let body = parseBlock('proc')
					const span = spanFromRange(parameters.span, body.span)
					return { kind: 'lambda', parameters, arrow, returnType, body, tags, span }
				} else {
					const open = take('operator', '(')
					const expr = parseExpression()
					const close = take('operator', ')')
					const span = spanFromRange(open.span, close.span)
					return { kind: 'parenthesized expression', open, expr, close, span }
				}
			}

			function parsePrimaryExpression() {
				switch (current().kind) {
					case 'operator': {
						const v = current().value
						if (v == '{') return parseBlock('expression')
						if (v == ';') return null
						if (v == '}') return null
						if (v == '(') {
							return parseParenthesizedExpressionOrLambda()
						}
						if (v == '[') {
							const list = parseList(parsePrimaryExpression, '[]')
							list.kind = 'array literal'
							return list
						}

						console.log(current())
						assert(false, `unexpected operator`)
					}
					case 'number': return take('number')
					case 'string': return parseStringLiteral()
					case 'symbol': {
						const keyword = current()
						switch (keyword.value) {
							case 'null':
								take('symbol')
								return { kind: 'null literal', span: keyword.span }
							case 'true':
								take('symbol')
								return { kind: 'boolean literal', value: true, span: keyword.span }
							case 'false':
								take('symbol')
								return { kind: 'boolean literal', value: false, span: keyword.span }
							default: {
								return parseSymbolExpression()
							}
						}
					}
					default: throw `unexpected token ${current().kind}::${current().value} for expression`
				}
			}
		}

		function parseTags() {
			const tags = []
			while (is('operator', '#')) {
				const hash = take('operator', '#')
				const tag = take('symbol')
				// TODO: maybe make this less gross
				let list
				if (is('operator', '(')) list = parseList(parseExpression)

				const last = list?.end.span ?? tag.span
				const span = spanFromRange(hash.span, last)
				const it = { kind: 'tag', hash, tag, value: tag.value, list, span }
				tags.push(it)
			}
			return tags
		}

		function parseExpressionOrDeclaration() {
			let it = parseDeclaration()
			if (!it) {
				it = parseExpression()
			}
			if (!it) throw 'failed to parse declaration or expression'
			return it
		}

		function parseDeclaration(takeTerminator = true) {
			const it = current()
			const cur = current().value
			if (cur != 'import') isTopLevel = false

			// find assignment before we parse all the other declarations
			// this allows us to assign variables with keyword names *without* escaping them 
			if (it.kind == 'symbol' && isAssignmentOperator(peek(1))) {
				return parseAssignment()
			}

			switch (cur) {
				case '#': {
					// found tags *before* declaration
					// this is only legal on blocks
					const tags = parseTags()
					return parseBlock(null, { tags })
				}
				case '{': return parseBlock(null)
				case 'pragma': {
					const keyword = take('symbol', 'pragma')
					const option = parseSymbol()
					const legalSymbols = new Set(['inc', 'lib', 'libpath'])
					assert(legalSymbols.has(option.value), `illegal pragma option ${option.value}, legal options are ${[...legalSymbols]}`)
					const name = parseStringLiteral()
					const span = spanFromRange(keyword.span, name.span)
					return { kind: 'pragma', keyword, option, name, span }
				}
				case 'import': {
					assert(isTopLevel, `imports should only be at top level`)
					const keyword = take('symbol', 'import')
					const path = parseStringLiteral()
					filesToImport.add(path)
					return { kind: 'import', keyword, path }
				}
				case 'use': {
					const keyword = take('symbol', 'use')
					const path = parsePath()
					const it = { kind: 'use', keyword, path }
					return it
				}
				case 'module': {
					const keyword = take('symbol', 'module')
					const name = take('symbol')
					let block
					if (is('operator', '{')) {
						block = parseBlock('module')
					} else {
						// don't parse brackets; take every remaining declaraction in the file >:)
						block = parseBlock('module', { brackets: false })
					}
					return { kind: 'module', keyword, name, block }
				}
				case 'type': {
					const keyword = take('symbol', 'type')
					const name = take('symbol')
					const equals = take('operator', '=')
					const type = parseType()
					return { kind: 'type alias', keyword, name, equals, type }
				}
				case 'proc': {
					const keyword = take('symbol', 'proc')
					const name = take('symbol')
					let parameters
					if (is('operator', '(')) {
						parameters = parseList(parseTypedSymbol)
					}
					let arrow, returnType
					if (is('operator', '->')) {
						arrow = take('operator', '->')
						returnType = parseType()
					}
					const tags = parseTags()
					let body
					if (is('operator', '{')) {
						body = parseBlock('proc')
					} else if (is('operator', ';')) {
						body = take('operator', ';')
					}
					return { kind: 'proc', keyword, name, parameters, arrow, returnType, body, tags }
				}
				case 'do': {
					const keyword = take('symbol', 'do')
					if (is('operator', '{')) {
						// do-while
						const block = parseBlock('do while')
						const whileKeyword = take('symbol', 'while')
						const condition = parseExpression()
						return { kind: 'do while', keyword, block, whileKeyword, condition }
					} else {
						// do block
						const name = take('symbol')
						let parameters
						if (is('operator', '(')) {
							parameters = parseList(parseTypedSymbol)
						}
						const tags = parseTags()
						let body = parseBlock('proc')
						return { kind: 'do', keyword, name, parameters, body, tags }
					}
				}
				case 'enum': {
					function parseEnumEntry() {
						// that's all for now, in the future we will have:
						// - person[name:string,age:int] // only legal if enum type is unspecified
						// - age = 10
						// - red[colorspace:string] = 0
						const name = parseSymbol()
						let params = null
						let equals, value = null
						if (is('operator', '(')) {
							params = parseList(parseTypedSymbol)
						} else if (is('operator', '=')) {
							equals = take('operator', '=')
							value = parseExpression()
						}
						return { kind: 'enum entry', name, params, equals, value }
					}
					const keyword = take('symbol')
					const name = parseSymbol()
					let colon, type, params

					if (is('operator', ':')) {
						colon = take('operator', ':')
						type = parseType()
					}

					if (is('operator', '(')) {
						params = parseList(parseTypedSymbol)
					}

					const tags = parseTags()
					const entries = parseList(parseEnumEntry, '{}')
					return { kind: 'enum', keyword, name, colon, type, params, tags, entries }
				}
				case 'union':
				case 'struct': {
					const keyword = take('symbol')
					const kind = keyword.value
					assert(kind == 'struct' || kind == 'union')

					const name = take('symbol')
					const parameters = parseList(parseTypedSymbol)
					const tags = parseTags()
					return { kind, keyword, name, parameters, tags }
				}
				case 'const':
				case 'var': {
					const keyword = take('symbol')
					const name = take('symbol')
					let colon, type
					if (is('operator', ':')) {
						colon = take('operator')
						type = parseType()
					}

					const tags = parseTags()

					let equals, expr, terminator
					if (is('operator', '=')) {
						equals = take('operator', '=')
						expr = parseExpression()
					}
					if (takeTerminator && is('operator', ';')) terminator = take('operator', ';')

					const last = terminator ?? expr ?? (tags.length ? tags[tags.length - 1] : null) ?? type ?? name
					if (!last.span) console.log(last)
					assert(last.span)
					const span = spanFromRange(keyword.span, last.span)

					return { kind: 'var', keyword, name, colon, type, equals, expr, terminator, tags, span }
				}
				case 'goto': {
					const keyword = take('symbol', 'goto')
					const label = take('symbol')
					let terminator
					if (takeTerminator && is('operator', ';')) {
						terminator = take('operator', ';')
					}
					return { kind: 'goto', keyword, label, terminator }
				}
				case 'label': {
					const keyword = take('symbol', 'label')
					const label = take('symbol')
					const colon = take('operator', ':')
					return { kind: 'label', keyword, label, colon }
				}
				case 'match': {
					const keyword = take('symbol', 'match')
					const operand = parseExpression()
					const begin = take('operator', '{')
					const arms = []

					function parseArm() {
						function parsePattern() {
							const expr = parseExpression()
							if (expr.kind == 'symbol') {
								return { kind: 'pattern equal', symbol: expr }
							}
							return { kind: 'pattern expression', expr }
						}
						const pattern = parsePattern()
						const block = parseExpressionOrDeclaration()
						return { kind: 'arm', pattern, block }
					}

					while (!isEndOfFile() && !is('operator', '}')) {
						const arm = parseArm()
						arms.push(arm)
					}
					const end = take('operator', '}')
					const it = { kind: 'match', keyword, operand, begin, arms, end }
					return it
				}
				case 'if': {
					const keyword = take('symbol', 'if')
					const condition = parseExpression()

					if (is('symbol', 'goto')) {
						const gotoKeyword = take('symbol', 'goto')
						const label = take('symbol')
						let terminator
						if (takeTerminator && is('operator', ';')) {
							terminator = take('operator', ';')
						}
						return { kind: 'goto', keyword: gotoKeyword, label, ifKeyword: keyword, condition, terminator }
					} else {
						const thenBlock = parseExpressionOrDeclaration()//('if', true, true)
						let elseKeyword, elseBlock
						if (is('symbol', 'else')) {
							elseKeyword = take('symbol', 'else')
							elseBlock = parseExpressionOrDeclaration()
						}

						return { kind: 'if', keyword, condition, thenBlock, elseKeyword, elseBlock }
					}
				}
				case 'while': {
					const keyword = take('symbol', 'while')
					// NOTE: condition may be parenthesized which allows for C-like while() syntax
					const condition = parseExpression()
					const block = parseBlock('while')
					return { kind: 'while', keyword, condition, block }
				}
				case 'for': {
					const keyword = take('symbol', 'for')

					let hasParens, begin, end
					let preCondition, terminator1, condition, terminator2, postCondition

					if (is('operator', '(')) {
						hasParens = true
						begin = take('operator', '(')
					}

					if (is('operator', ';')) {
						terminator1 = take('operator', ';')
					} else {
						preCondition = parseDeclaration(false)
						terminator1 = take('operator', ';')
					}

					if (is('operator', ';')) {
						terminator2 = take('operator', ';')
					} else {
						condition = parseExpression()
						terminator2 = take('operator', ';')
					}

					if ((hasParens && !is('operator', ')')) || !is('operator', '{')) {
						postCondition = parseExpression()
					}

					if (hasParens) {
						end = take('operator', ')')
					}

					// TODO: don't allow 'real' declarations in for statement block, just control flow stuff
					const block = parseBlock('for')
					return { kind: 'for', keyword, begin, preCondition, terminator1, condition, terminator2, postCondition, end, block }
				}
				case 'each': {
					const keyword = take('symbol', 'each')
					let hasParens, begin, end
					if (is('operator', '(')) {
						hasParens = true
						begin = take('operator', '(')
					}
					const item = take('symbol')
					let comma, index
					if (is('operator', ',')) {
						comma = take('operator', ',')
						index = take('symbol')
					}
					const from = take('operator', '<-')
					const list = take('symbol')
					if (hasParens) {
						end = take('operator', ')')
					}
					const block = parseBlock('each')

					return { kind: 'each', keyword, begin, item, comma, index, from, list, end, block }
				}
				case 'return': {
					const keyword = take('symbol', 'return')
					let expr, terminator

					if (is('operator', ';')) {
						if (is('operator', ';')) terminator = take('operator', ';')
					} else {
						expr = parseExpression()
						if (is('operator', ';')) terminator = take('operator', ';')
					}

					return { kind: 'return', keyword, expr, terminator }
				}
				case 'break': {
					const keyword = take('symbol')
					return { kind: 'break', span: keyword.span }
				}
				case 'continue': {
					const keyword = take('symbol')
					return { kind: 'continue', span: keyword.span }
				}
				default:
					return false
			}
		}

		function parsePath() {
			return take('symbol')
		}

		function parseSymbol() {
			const symbol = take('symbol')
			if (symbol.value.startsWith('`')) {
				// TODO: keep the ` and ignore it while doing symbol lookups in the binder
				// this would allow us to properly reconstruct source code from the ast
				symbol.value = symbol.value.replace(/`/g, '')
			}
			return symbol
		}

		function parseTypedSymbol() {
			let spread
			if (is('operator', '...')) {
				spread = take('operator', '...')
			}
			const name = parseSymbol()
			const colon = take('operator', ':')
			const type = parseType()
			const tags = parseTags()

			return { kind: 'typed symbol', spread, name, colon, type, tags }
		}

		function parseList(itemParser, bookend = '()') {
			assert(bookend && bookend.length == 2, 'the list bookend consists of an opening an closing character')

			const items = []
			const begin = take('operator', bookend[0])
			while (!is('operator', bookend[1])) {
				const item = itemParser()
				if (!item) throw 'failed to parse list item'
				items.push(item)
				if (!is('operator', bookend[1])) {
					items.push(take('operator', ','))
				}
			}
			const end = take('operator', bookend[1])
			const span = spanFromRange(begin.span, end.span)
			return { kind: 'list', begin, items, end, span }
		}

		function parseBlock(scope, options = {}) {
			const defaults = { withBrackets: true, tags: null }
			options = { ...defaults, ...options }

			const statements = []

			const { withBrackets, tags } = options

			let begin
			if (withBrackets) begin = take('operator', '{')

			while (!isEndOfFile() && !is('operator', '}')) {
				const stmt = parseDeclaration(scope)
				if (stmt) {
					statements.push(stmt)
					continue
				}

				const allowsTermination = new Set(['number', 'string', 'array literal', 'binary', 'unary', 'assignment', 'call', 'property access'])
				let expr = parseExpression()
				if (expr) {
					if (is('operator', ';') && allowsTermination.has(expr.kind)) {
						expr = { kind: 'terminated expression', expr, terminator: take('operator', ';') }
					}

					statements.push(expr)
					continue
				}

				console.log(current())
				throw 'failed to parse statement or expression in block'
			}

			let end
			if (withBrackets) end = take('operator', '}')

			let span

			if (withBrackets) {
				span = spanFromRange(begin.span, end.span)
			}

			return { kind: 'block', tags, begin, statements, end, span }
		}

		function parseChain() {
			let lhs = parseSymbol()
			while (is('operator', '.')) {
				const dot = take('operator', '.')
				const rhs = parseSymbol()
				const span = spanFromRange(lhs.span, rhs.span)
				lhs = { kind: 'property access', scope: lhs, dot, property: rhs, span }
			}

			return lhs
		}
		function parseType() {
			if (is('symbol')) {
				const name = parseChain()
				return { kind: 'type atom', name, span: name.span }
			} else if (is('operator', '[')) {
				let size
				let begin = take('operator', '[')

				if (!is('operator', ']')) {
					size = parseExpression()
				}

				let end = take('operator', ']')
				let of = parseType()
				const span = spanFromRange(begin.span, of.span)
				return { kind: 'type array', begin, size, end, of, span }
			} else if (is('operator', '~>')) {
				let pointer = take('operator', '~>')
				let to = parseType()
				const span = spanFromRange(pointer.span, to.span)
				return { kind: 'type pointer', pointer, to, span }
			} else if (is('operator', '?')) {
				let optional = take('operator', '?')
				let it = parseType()
				const span = spanFromRange(optional.span, it.span)
				return { kind: 'type optional', optional, it, span }
			} else if (is('operator', '!')) {
				let mutable = take('operator', '!')
				let it = parseType()
				const span = spanFromRange(mutable.span, it.span)
				return { kind: 'type mutable', mutable, it, span }
			} else if (is('operator', '(')) {
				// always a function for now, might be a tuple eventually
				const params = parseList(parseType)
				const arrow = take('operator', '->')
				const returnType = parseType()
				const span = spanFromRange(params.span, returnType.span)
				return { kind: 'type function', params, arrow, returnType, span }
			}
			console.log(current())
			throw `unhandled type ${current().kind}::${current().value}`

		}
	}
}

module.exports = { parse, fileMap }
