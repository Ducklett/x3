const { parse } = require('./parser')
const { bind, lower } = require('./bind')
const { inspect } = require('util')
const { emitAsm, read } = require('./compiler')
const { outputHtml } = require('./tool/outputHtml')

compile('examples/10_snake.x3')
// compile('examples/coreutils/echo.x3')

function compile(filename) {
	const file = ({ path: filename, code: read(filename) })
	const syntaxTree = parse(file)

	const ast = bind(syntaxTree)
	const [loweredAst, meta] = lower(ast)
	const entrypoint = meta.entrypoint?.name || 'main'
	emitAsm(loweredAst, { entrypoint })

	// outputHtml(syntaxTree)

	// console.log(inspect(ast, { depth: 2 }))
}
