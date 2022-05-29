const chalk = require('chalk')
const { tag_int, tag_float } = require('./ast')
const { getFlag } = require('./compiler')
const { assert, read } = require("./util")

const error = {
	// lexer
	expectedSymbol(char, span) { return { err: 'expectedSymbol', char, span } },

	// parser
	expectedStatement(token) { return { err: 'expectedStatement', token } },

	// binder
	expectedDeclaration(node) { return { err: 'expectedDeclaration', node } },
	symbolNotFound(token) { return { err: 'symbolNotFound', token } },
	symbolForAssignmentNotFound(node) { return { err: 'symbolForAssignmentNotFound', node } },
	noInitOnVariableWithInitializer(node) { return { err: 'noInitOnVariableWithInitializer', node } },
	variableWithoutInitializer(node) { return { err: 'variableWithoutInitializer', node } },
	typeMismatch(type, node) { return { err: 'typeMismatch', type, node } },
	unsupportedTypeForIndexing(node) { return { err: 'unsupportedTypeForIndexing', node } },
}

const errors = []

function reportError(err) {
	const index = errors.length
	errors.push(err)
	return index
}

function errorKindForIndex(index) {
	if (index >= errors.length) throw 'out of bounds'
	return errors[index].err
}

function upgradeError(index, newError) {
	if (index >= errors.length) throw 'out of bounds'
	errors[index] = newError
	return index
}

function hasErrors() { return errors.length > 0 }

function displayErrors() {
	const boring = getFlag('boring')
	if (!boring) console.log(`${'     compiler has errors:'.toUpperCase()}`)
	for (let error of errors) {
		assert(error.err)
		renderError(error, boring)
		if (!boring) console.log('')
	}
	if (!boring) console.log(`     ┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄`)
	process.exit(1)
}

function renderError(error, boring) {

	// TODO: optimize file reads
	function textFromSpan(span) {
		const source = read(span.file)
		assert(source)
		return source.slice(span.from, span.to)
	}

	function renderSpan(span) {
		if (boring) {
			return `${span.file}:${span.fromLine + 1},${span.fromColumn + 1},${span.toLine},${span.toColumn}`
		} else {
			return `${span.file}:${span.fromLine + 1}:${span.fromColumn + 1}`
		}
	}

	const span = error.span ?? error.token?.span ?? error.node?.span
	assert(span)

	if (!boring) {
		console.log(`${chalk.red('╭───')} ${chalk.underline(renderSpan(span))}`)
	}

	function reason(str) {
		if (boring) {
			console.log(`${renderSpan(span)} ${str}`)
		} else {
			console.log(`${chalk.red('╰──►')} ${str}`)
		}
	}

	function help(str) {
		if (!boring) console.log(`     ┊ ${str}`)
	}

	switch (error.err) {
		// lexer
		case 'expectedSymbol':
			reason(`expected an identifier but got character '${error.char}'.`)
			break

		// parser
		case 'expectedStatement':
			reason(`expected a statement but got ${error.token.kind} '${error.token.value}'.`)
			break

		// binder
		case 'expectedDeclaration':
			switch (error.node.kind) {
				case 'reference': {
					const symbol = error.node.symbol
					assert(symbol)
					const name = chalk.red(symbol.name)
					reason(`expected a declaration, but found a reference to '${name}'.`)
					switch (symbol.kind) {
						case 'declareVar':
							help(`'${name}' is a variable, prehaps you wanted to do one of the following:`)
							help(`return it:    ${chalk.magenta('return')} ${chalk.yellow(symbol.name)}`)
							if (symbol.type.tag == tag_int || symbol.type.tag == tag_float) {
								help(`increment it: ${chalk.red(symbol.name)}${chalk.cyan('++')}`)
							}
							help(`assign to it: ${chalk.red(symbol.name)} ${chalk.cyan('=')} _`)
							break
						case 'function':
							help(`'${name}' is a function, prehaps you meant to call it:`)
							help(`${chalk.blue(symbol.name)}()`)
							break
						default: throw `unhandled kind ${symbol.kind}`
					}
				} break

				case 'binary':
				case 'parenthesized expression':
				case 'property access':
				case 'binary': {
					function humanReadable(k) {
						switch (k) {
							case 'binary': return 'binary expression'
							case 'parenthesized expression': return 'parenthesized expression'
							case 'property access': return 'property access'
							default: throw `unhandled ${k}`
						}
					}
					const readableKind = humanReadable(error.node.kind)
					reason(`expected a declaration, but found a ${chalk.cyan(readableKind)}.`)
					help(`this expression is not allowed here because it is generally pure`)
					help(`to allow it you should explicitly discard the result:`)
					const expressionText = textFromSpan(error.node.span)
					// TODO: proper syntax highlighting for expression
					help(`_ ${chalk.cyan('=')} ${chalk.yellow(expressionText)}`)
				} break
				default: throw `unhandled kind ${error.node.kind}`
			}
			break
		case 'symbolNotFound': {
			const name = error.token.value
			reason(`could not resolve symbol of name '${chalk.red(name)}'.`)
			help(`perhaps you are missing a '${chalk.magenta('use')}' statement or made a typo`)
		} break
		case 'symbolForAssignmentNotFound': {
			// var x = foo
			//     |
			//     they forgot the name, so the statement was reinterpreted as 'var = foo'
			const probablyMissingName = new Set(['type', 'var', 'const'])
			const name = error.node.varDec.value
			const expr = textFromSpan(error.node.expr.span)

			reason(`could not resolve symbol '${chalk.red(name)}' in assignment.`)
			if (probablyMissingName.has(name)) {
				help(`perhaps you were trying to create a ${name} declaration?`)
				help(`${chalk.magenta(name)} ${chalk.underline.red('foo')} ${chalk.cyan('=')} ${chalk.yellow(expr)}`)
			} else {
				help(`perhaps you want to declare a new ${chalk.magenta('var')} instead:`)
				help(`${chalk.underline.magenta('var')} ${chalk.red(name)} ${chalk.cyan('=')} ${chalk.yellow(expr)}`)
			}
		} break
		case 'noInitOnVariableWithInitializer':
			reason(`'${chalk.red(error.node.name.value)}' is marked as ${chalk.yellow('#noinit')} but it has an initializer.`)
			help(`either remove the ${chalk.yellow('#noinit')} tag or remove the inititalizer`)
			break
		case 'variableWithoutInitializer':
			reason(`'${chalk.red(error.node.name.value)}' is not initialized`)
			help(`either initialize it or add the ${chalk.yellow('#noinit')} tag`)
			break
		case 'typeMismatch':
			const expected = error.type
			const got = error.node.type
			// console.log(error)
			reason(`expected an expression of type ${formatType(expected)} but got ${formatType(got)}`)
			break
		case 'unsupportedTypeForIndexing':
			const indexableTypes = ['string', 'cstring', 'array', 'buffer', 'pointer']
			const typesFormatted = indexableTypes.map(t => chalk.bold.yellow(t)).join(', ')
			reason(`expression is of type ${formatType(error.node.type)}, it cannot be indexed.`)
			help(`the types that can be indexed are: ${typesFormatted}`)
			break
		default: throw `unhandled error code ${error.err}`
	}
}

function formatType(t) {
	switch (t.type) {
		case 'char':
		case 'string': case 'cstring':
		case 'u64': case 'i64':
		case 'u32': case 'i32':
		case 'u16': case 'i16':
		case 'u8': case 'i8':
		case 'uint': case 'int':
			return chalk.red(t.type)
		case 'buffer': return `[${chalk.yellow(t.count)}]${formatType(t.of)}`
		case 'array': return `[]${formatType(t.of)}`
		default: throw `unhandled ${t.type}`
	}
}
module.exports = { error, reportError, errorKindForIndex, upgradeError, hasErrors, displayErrors }
