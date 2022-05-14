const { read, assert } = require('./compiler')
const path = require("path")

const fileMap = new Map()

function spanFromRange(from, to) {
	return { ...from, to: to.to }
}

function parse(source) {

	fileMap.set(source.path, source.code)

	const code = source.code

	const keywords = new Set(["module", "import", "use", "type", "struct", "union", "proc", "scope", "return", "break", "continue", "goto", "label", "var", "const", "for", "do", "while", "each", "enum", "if", "else", "match", "true", "false"])
	const operators = new Set(["...", "<<=", ">>=", "&&=", "||=", "==", "!=", ">=", "<=", "<<", ">>", "<~", "~>", "->", "=>", "&&", "||", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=", "!", ">", "<", "+", "-", "/", "*", "%", "^", "~", "&", "|", "(", ")", "[", "]", "{", "}", "?", ":", ";", ".", ","])
	const binaryOperators = new Set(["==", "!=", ">=", "<=", "<<", ">>", "&&", "||", "=>", ">", "<", "+", "-", "/", "*", "%", "^", "&", "|"])
	const preUnaryOperators = new Set(["++", "--", "!", "-", "~>", "<~"])
	const postUnaryOperators = new Set(["++", "--"])
	const assignmentOperators = new Set(["<<=", ">>=", "&&=", "||=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=",])

	const tokens = lex(code)
	const statements = _parse(tokens)
	//emitHtml(statements)
	//console.log(JSON.stringify(statements, null, 2))
	return statements

	function lex(code) {
		let span
		function startSpan() {
			span = { file: source.path, from: lexerIndex }
		}

		function takeSpan() {
			span.to = lexerIndex
			return span
		}

		function isNewline(c) { return c == '\n' }

		function isWhitespace(c) {
			return c == ' ' || c == "\n" || c == "\r" || c == '\t'
		}

		function isLegalKeyword(c) {
			return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
		}

		function isLegalSymbol(c) {
			return c == ' ' || c == '_' || isLegalKeyword(c) || (c >= '0' && c <= '9')
		}

		function isLegalNumber(c) {
			return (c >= 0 && c <= 9)
		}

		function isLegalHexNumber(c) {
			return (c >= 0 && c <= 9) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
		}

		function isLegalOctalNumber(c) {
			return (c >= 0 && c <= 7)
		}

		function isLegalBinaryNumber(c) {
			return (c == 0 || c == 1)
		}


		const maxTokenLength = 3;
		let lexerIndex = 0;
		const len = code.length

		let tokens = []


		lex:
		while (lexerIndex < len) {

			const current = () => {
				// overstepped by 2, should definitely just kys now to prevent inifinite loop ;)
				if (lexerIndex > len) throw "out of range!"
				return code[lexerIndex]
			}

			const peek = (n) => {
				return code[lexerIndex + n]
			}

			const lexSymbol = (from = null, leftEscaped = false) => {
				let rightEscaped = false

				if (!from) {
					if (current() == "'") {
						lexerIndex++
						leftEscaped = true
					}
					from = lexerIndex
				}

				while (isLegalSymbol(current())) lexerIndex++

				// remove trailing whitespace
				while (isWhitespace(peek(-1))) lexerIndex--


				let symbol = code.slice(from, lexerIndex)

				// symbols may end with ' to prevent collisions with keywords
				if (current() == "'") {
					lexerIndex++
					rightEscaped = true
				}

				return { kind: 'symbol', value: symbol, leftEscaped, rightEscaped, span: takeSpan() }
			}

			startSpan()
			let from = lexerIndex
			while (isWhitespace(current())) {
				if (isNewline(current())) {
					// TODO: also keep track of the line number so we can do proper error messages with them :)
					if (tokens.length > 0 && tokens[tokens.length - 1].kind == 'symbol') {
						const smb = tokens[tokens.length - 1]
						// implicit right escape by inserting a newline
						smb.rightEscaped = true
					}

				}
				lexerIndex++
			}
			if (from != lexerIndex) {
				tokens.push({ kind: 'whitespace', value: code.slice(from, lexerIndex), span: takeSpan() })
			}

			if (code[lexerIndex] === undefined) break

			startSpan()

			// multi line comment
			if (current() == '/' && peek(1) == '*') {
				let from = lexerIndex + 2
				while (!(current() == '*' && peek(1) == '/')) lexerIndex++
				const comment = code.slice(from, lexerIndex)
				lexerIndex += 2
				tokens.push({ kind: 'comment', value: comment, span: takeSpan() })
				continue
			}

			// single line comment
			if (current() == '/' && peek(1) == '/') {
				let from = lexerIndex + 2
				while (!(current() == '\n')) lexerIndex++
				const comment = code.slice(from, lexerIndex)
				tokens.push({ kind: 'comment', value: comment, span: takeSpan() })
				continue
			}

			// tag
			if (current() == '#') {
				lexerIndex++
				const smb = lexSymbol()
				if (!smb) throw 'expected symbol..'
				const value = smb.value
				tokens.push({ kind: 'tag', value, span: takeSpan() })
				continue
			}

			//string
			let shouldNullTerminate = false
			if (current() == 'c' && peek(1) == '"') {
				shouldNullTerminate = true
				lexerIndex++
			}

			if (current() == '"') {
				lexerIndex++
				let from = lexerIndex
				while (true) {
					if (current() == '"') break
					if (current() == '\\' && peek(1) == '"') {
						lexerIndex += 2
					}
					else {
						lexerIndex++
					}
				}

				let str = code.slice(from, lexerIndex)
				// TODO: maybe move this elsewhere
				str = str.replace(/\\n/g, '\n')
					.replace(/\\r/g, '\r')
					.replace(/\\t/g, '\t')
					.replace(/\\0/g, '\0')
					.replace(/\\"/g, '"')
					// arbitrary hex byte
					.replace(/\\x([0-9a-zA-Z]{2})/g, (...p) => String.fromCharCode(parseInt(p[1], 16)))
				if (shouldNullTerminate) str += '\0'
				lexerIndex++
				tokens.push({ kind: 'string', value: str, span: takeSpan() })
				continue
			}

			// number
			if (isLegalNumber(current())) {
				let from = lexerIndex
				if (current() == '0' && peek(1) == 'b') {
					// binary
					lexerIndex += 2
					let from = lexerIndex
					while (isLegalBinaryNumber(current())) lexerIndex++

					const slice = code.slice(from, lexerIndex)
					let num = parseInt(slice, 2)
					tokens.push({ kind: 'number', value: num, span: takeSpan() })
					continue
				}
				else if (current() == '0' && peek(1) == 'x') {
					// hex
					lexerIndex += 2
					let from = lexerIndex
					while (isLegalHexNumber(current())) lexerIndex++

					const slice = code.slice(from, lexerIndex)
					let num = parseInt(slice, 16)
					tokens.push({ kind: 'number', value: num, span: takeSpan() })
					continue
				}
				else if (current() == '0' && peek(1) == 'o') {
					// octal
					lexerIndex += 2
					let from = lexerIndex
					while (isLegalOctalNumber(current())) lexerIndex++

					const slice = code.slice(from, lexerIndex)
					let num = parseInt(slice, 8)
					tokens.push({ kind: 'number', value: num, span: takeSpan() })
					continue

				} else {
					// decimal
					while (isLegalNumber(current())) lexerIndex++
					// float
					if (current() == '.') {
						lexerIndex++
						while (isLegalNumber(current())) lexerIndex++
					}
					let num = parseFloat(code.slice(from, lexerIndex))
					tokens.push({ kind: 'number', value: num, span: takeSpan() })
					continue
				}
			}

			// symbols may start with ' to prevent collisions with keywords
			var mightBeKeyword = true
			if (current() == "'" && isLegalSymbol(peek(1))) {
				lexerIndex++
				mightBeKeyword = false
			}

			// keyword and symbol
			if (isLegalKeyword(current()) || isLegalSymbol(current())) {
				let from = lexerIndex

				if (mightBeKeyword) {
					while (isLegalKeyword(current())) lexerIndex++
					if (current() != "'") {
						let potentialKeyword = code.slice(from, lexerIndex)
						if (keywords.has(potentialKeyword)) {
							tokens.push({ kind: 'keyword', value: potentialKeyword, span: takeSpan() })
							continue
						}
					}
				}

				const value = lexSymbol(from)
				tokens.push(value)
				continue
			}

			// operator
			{
				let from = lexerIndex
				let operatorLen = maxTokenLength
				while (operatorLen) {
					const op = code.slice(from, from + operatorLen)
					if (operators.has(op)) {
						lexerIndex = from + operatorLen
						tokens.push({ kind: 'operator', value: op, span: takeSpan() })
						continue lex;
					}
					operatorLen--
				}
			}

			throw `unexpected character '${code[lexerIndex]} : ${lexerIndex}'`
		}

		return tokens
	}

	function _parse(tokens) {
		let isTopLevel = true
		const filesToImport = new Set()
		let lastSymbol = null
		tokens = tokens.filter((t, i, arr) => {
			if (t.kind == 'whitespace') {
				if (t.value.length > 1) {
					if (lastSymbol) {
						lastSymbol.rightEscaped = true
					}
				}
				return false
			} else if (t.kind == 'comment') {
				if (lastSymbol) {
					lastSymbol.rightEscaped = true
				}
				return false
			} else if (t.kind == 'symbol' || t.kind == 'keyword') {
				lastSymbol = t
			} else {
				lastSymbol = null
			}
			return true
		})
		let tokenIndex = 0

		let contents = parseFile()
		const files = [contents]
		for (let filePath of filesToImport) {
			assert(filePath.value.startsWith('./'), `import file path always starts with ./`)
			let p = filePath.value.slice(2)
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
		function current() { return peek(0) }

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
		function isAssignmentOperator() {
			const cur = current()
			return cur.kind == 'operator' && assignmentOperators.has(cur.value)
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
			expect(kind, value)
			return tokens[tokenIndex++]
		}

		function parseFile() {
			const declarations = []
			while (true) {
				if (!current()) break
				const cur = current()
				const stmt = parseDeclaration('file')
				if (!stmt) {
					console.log(cur)
					assert(false, 'expected statement')
				}
				declarations.push(stmt)
			}

			assert(!current(), "all tokens consumed at end of file")
			return { kind: 'file', declarations }
		}

		function parseExpression() {
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

			while (currentIsBinaryOperator()) {
				const op = take('operator')
				// TODO: precedence climbing
				// NOTE: only the first part of parseExpression can currently have unary operator
				const rhs = parsePrimaryExpression()
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

					if (isAssignmentOperator(current())) {
						const operator = take('operator')
						const expr = parseExpression()
						const span = spanFromRange(lhs.span, expr.span)
						lhs = { kind: 'assignment', name: lhs, operator, expr, span }
						continue
					}

					if (is('operator', ':')) {
						const colon = take('operator', ':')
						const alias = parseSymbol()
						const span = spanFromRange(lhs.span, alias.span)
						lhs = { kind: 'alias', name: lhs, colon, alias, span }
						break
					}

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
					if (is('symbol') || is('keyword')) {
						parseSymbol()
						if (is('operator', ':')) {
							take('operator', ':')
							parseType()
							if (is('operator', ',')) {
								isLambda = true
							} else if (is('operator', ')')) {
								take('operator', ')')
								if (is('tag') || is('operator', '->') || is('operator', '{')) {
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
					let body = parseBlock('proc', true, true)
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
						if (v == '{') return parseBlock('expression', true, true)
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
					case 'string': return take('string')

					case 'symbol': return parseSymbolExpression()
					case 'keyword': {
						const keyword = current()
						switch (keyword.value) {
							case 'true':
								take('keyword')
								return { kind: 'boolean literal', value: true, span: keyword.span }
							case 'false':
								take('keyword')
								return { kind: 'boolean literal', value: false, span: keyword.span }
							case 'break':
								take('keyword')
								return { kind: 'break', span: keyword.span }
							case 'continue':
								take('keyword')
								return { kind: 'continue', span: keyword.span }
							case 'return': {
								take('keyword')
								const expr = parseExpression()
								const span = spanFromRange(keyword.span, expr.span)
								return { kind: 'return', keyword, expr, span }
							}
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
			while (is('tag')) {
				const tag = take('tag')
				// TODO: maybe make this less gross
				let list
				if (is('operator', '(')) list = parseList(parseExpression)
				tag.list = list
				tags.push(tag)
			}
			return tags
		}

		function parseDeclaration(takeTerminator = true) {
			const cur = current().value
			if (cur != 'import') isTopLevel = false

			switch (cur) {
				case 'import': {
					assert(isTopLevel, `imports should only be at top level`)
					const keyword = take('keyword', 'import')
					const path = take('string')
					filesToImport.add(path)
					return { kind: 'import', keyword, path }
				}
				case 'use': {
					const keyword = take('keyword', 'use')
					const path = parsePath()
					const it = { kind: 'use', keyword, path }
					return it
				}
				case 'module': {
					const keyword = take('keyword', 'module')
					const name = take('symbol')
					let block
					if (is('operator', '{')) {
						block = parseBlock('module', true, false)
					} else {
						// don't parse brackets; take every remaining declaraction in the file >:)
						block = parseBlock('module', true, false, false)
					}
					return { kind: 'module', keyword, name, block }
				}
				case 'type': {
					const keyword = take('keyword', 'type')
					const name = take('symbol')
					const equals = take('operator', '=')
					const type = parseType()
					return { kind: 'type alias', keyword, name, equals, type }
				}
				case 'proc': {
					const keyword = take('keyword', 'proc')
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
						body = parseBlock('proc', true, true)
					} else if (is('operator', ';')) {
						body = take('operator', ';')
					}
					return { kind: 'proc', keyword, name, parameters, arrow, returnType, body, tags }
				}
				case 'scope': {
					const keyword = take('keyword', 'scope')
					const name = take('symbol')
					let parameters
					if (is('operator', '(')) {
						parameters = parseList(parseTypedSymbol)
					}
					const tags = parseTags()
					let body = parseBlock('proc', true, true)
					return { kind: 'scope', keyword, name, parameters, body, tags }
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
					const keyword = take('keyword')
					const name = parseSymbol()
					let params
					if (is('operator', '(')) {
						params = parseList(parseTypedSymbol)
					}
					const tags = parseTags()
					const entries = parseList(parseEnumEntry, '{}')
					return { kind: 'enum', keyword, name, params, tags, entries }
				}
				case 'union':
				case 'struct': {
					const keyword = take('keyword')
					const kind = keyword.value
					assert(kind == 'struct' || kind == 'union')

					const name = take('symbol')
					const parameters = parseList(parseTypedSymbol)
					const tags = parseTags()
					return { kind, keyword, name, parameters, tags }
				}
				case 'const':
				case 'var': {
					const keyword = take('keyword')
					const name = take('symbol')
					let colon, type
					if (is('operator', ':')) {
						colon = take('operator')
						type = parseType()
					}
					let equals, expr, terminator
					if (is('operator', '=')) {
						equals = take('operator', '=')
						expr = parseExpression()
					}
					const tags = parseTags()
					if (takeTerminator && is('operator', ';')) terminator = take('operator', ';')
					return { kind: 'var', keyword, name, colon, type, equals, expr, terminator, tags }
				}
				case 'goto': {
					const keyword = take('keyword', 'goto')
					const label = take('symbol')
					let terminator
					if (takeTerminator && is('operator', ';')) {
						terminator = take('operator', ';')
					}
					return { kind: 'goto', keyword, label, terminator }
				}
				case 'label': {
					const keyword = take('keyword', 'label')
					const label = take('symbol')
					const colon = take('operator', ':')
					return { kind: 'label', keyword, label, colon }
				}
				case 'match': {
					const keyword = take('keyword', 'match')
					const operand = parseExpression()
					const begin = take('operator', '{')
					const arms = []

					function parseArm() {
						function parsePattern() {
							// only support enum equality for now
							const symbol = parseSymbol()
							let colon, alias
							if (is('operator', ':')) {
								colon = take('operator', ':')
								alias = parseSymbol()
							}

							return { kind: 'pattern equal', symbol, colon, alias }
						}
						const pattern = parsePattern()
						const block = parseBlock('arm', true, true, true)
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
					const keyword = take('keyword', 'if')
					const condition = parseExpression()
					if (is('keyword', 'goto')) {
						const gotoKeyword = take('keyword', 'goto')
						const label = take('symbol')
						let terminator
						if (takeTerminator && is('operator', ';')) {
							terminator = take('operator', ';')
						}
						return { kind: 'goto', keyword: gotoKeyword, label, ifKeyword: keyword, condition, terminator }
					} else {
						const thenBlock = parseBlock('if', true, true)
						let elseKeyword, elseBlock
						if (is('keyword', 'else')) {
							elseKeyword = take('keyword', 'else')
							if (is('keyword', 'if')) {
								elseBlock = parseDeclaration()
							} else {
								// TODO: don't allow 'real' declarations in if statement block, just control flow stuff
								elseBlock = parseBlock('if', true, true)
							}
						}

						return { kind: 'if', keyword, condition, thenBlock, elseKeyword, elseBlock }
					}
				}
				case 'while': {
					const keyword = take('keyword', 'while')
					// NOTE: condition may be parenthesized which allows for C-like while() syntax
					const condition = parseExpression()
					// TODO: don't allow 'real' declarations in while statement block, just control flow stuff
					const block = parseBlock('while', true, true)
					return { kind: 'while', keyword, condition, block }
				}
				case 'do': {
					const keyword = take('keyword', 'do')
					// TODO: don't allow 'real' declarations in do while statement block, just control flow stuff
					const block = parseBlock('do while', true, true)
					const whileKeyword = take('keyword', 'while')
					const condition = parseExpression()
					return { kind: 'do while', keyword, block, whileKeyword, condition }
				}
				case 'for': {
					const keyword = take('keyword', 'for')

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
					const block = parseBlock('for', true, true)
					return { kind: 'for', keyword, begin, preCondition, terminator1, condition, terminator2, postCondition, end, block }
				}
				case 'each': {
					const keyword = take('keyword', 'each')
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
					const colon = take('operator', ':')
					const list = take('symbol')
					if (hasParens) {
						end = take('operator', ')')
					}
					const block = parseBlock('each', true, true)

					return { kind: 'each', keyword, begin, item, comma, index, colon, list, end, block }
				}
				case 'return': {
					const keyword = take('keyword', 'return')
					let expr, terminator

					if (is('operator', ';')) {
						if (is('operator', ';')) terminator = take('operator', ';')
					} else {
						expr = parseExpression()
						if (is('operator', ';')) terminator = take('operator', ';')
					}

					return { kind: 'return', keyword, expr, terminator }
				}
				default:
					return false
			}
		}

		function parsePath() {
			return take('symbol')
		}

		function parseSymbol(root = true) {
			let symbol

			if (is('keyword')) {
				symbol = take('keyword')
				symbol.kind = 'symbol'

			} else if (is('symbol')) {
				symbol = take('symbol')
			}

			if (symbol && !symbol?.rightEscaped) {
				const toTheRight = parseSymbol(false)
				if (toTheRight) {
					symbol.value += ' ' + toTheRight.value
				}
			}

			if (root && !symbol) {
				console.log(current())
				throw 'expected symbol!'
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

		function parseBlock(scope, allowDeclarations, allowExpressions, withBrackets = true) {
			const statements = []
			let begin
			if (withBrackets) begin = take('operator', '{')

			while (!isEndOfFile() && !is('operator', '}')) {
				if (allowDeclarations) {
					const stmt = parseDeclaration(scope)
					if (stmt) {
						statements.push(stmt)
						continue
					}
				}
				if (allowExpressions) {
					// TODO: looks like i'm handling return twice; fix this?
					const allowsTermination = new Set(['number', 'string', 'array literal', 'binary', 'unary', 'assignment', 'call', 'return', 'property access'])
					let expr = parseExpression()
					if (expr) {
						if (is('operator', ';') && allowsTermination.has(expr.kind)) {
							expr = { kind: 'terminated expression', expr, terminator: take('operator', ';') }
						}

						statements.push(expr)
						continue
					}
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

			return { kind: 'block', begin, statements, end, span }
		}

		function parseChain() {
			let lhs = parseSymbol()
			while (is('operator', '.')) {
				const dot = take('operator', '.')
				const rhs = parseSymbol()
				lhs = { kind: 'property access', scope: lhs, dot, property: rhs }
			}

			return lhs
		}
		function parseType() {
			if (is('symbol') || is('keyword')) {
				return { kind: 'type atom', name: parseChain() }
			} else if (is('operator', '[')) {
				let size
				let begin = take('operator', '[')

				if (is('number')) size = take('number')
				else if (is('symbol') || is('keyword')) size = parseSymbol()

				let end = take('operator', ']')
				let of = parseType()
				return { kind: 'type array', begin, size, end, of }
			} else if (is('operator', '~>')) {
				let pointer = take('operator', '~>')
				let to = parseType()
				return { kind: 'type pointer', pointer, to }
			} else if (is('operator', '!')) {
				let mutable = take('operator', '!')
				let it = parseType()
				return { kind: 'type mutable', mutable, it }
			} else if (is('operator', '(')) {
				// always a function for now, might be a tuple eventually
				const params = parseList(parseType)
				const arrow = take('operator', '->')
				const returnType = parseType()
				return { kind: 'type function', params, arrow, returnType }
			}
			console.log(current())
			throw `unhandled type ${current().kind}::${current().value}`

		}
	}
}

module.exports = { parse, fileMap }
