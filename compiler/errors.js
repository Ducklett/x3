const chalk = require('chalk')
const { tag_int, tag_float } = require('./ast')
const { assert, read } = require("./util")

const error = {
	// lexer
	expectedSymbol(char, span) { return { err: 'expectedSymbol', char, span } },

	// parser
	expectedStatement(token) { return { err: 'expectedStatement', token } },

	// binder
	expectedDeclaration(node) { return { err: 'expectedDeclaration', node } },
	symbolNotFound(token) { return { err: 'symbolNotFound', token } },
	noInitOnVariableWithInitializer(node) { return { err: 'noInitOnVariableWithInitializer', node } },
	variableWithoutInitializer(node) { return { err: 'variableWithoutInitializer', node } },
}

const errors = []

function reportError(err) {
	errors.push(err)
}
function hasErrors() { return errors.length > 0 }

function displayErrors() {
	const boring = process.argv[2] == '--boring'
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
		case 'symbolNotFound':
			reason(`could not resolve symbol of name '${error.token.value}'.`)
			help(`perhaps you are missing a '${chalk.magenta('use')}' statement or made a typo`)
			break
		case 'noInitOnVariableWithInitializer':
			reason(`'${chalk.red(error.node.name.value)}' is marked as ${chalk.yellow('#noinit')} but it has an initializer.`)
			help(`either remove the ${chalk.yellow('#noinit')} tag or remove the inititalizer`)
			break
		case 'variableWithoutInitializer':
			reason(`'${chalk.red(error.node.name.value)}' is not initialized`)
			help(`either initialize it or add the ${chalk.yellow('#noinit')} tag`)
			break
		default: throw `unhandled error code ${error.err}`
	}
}

module.exports = { error, reportError, hasErrors, displayErrors }
