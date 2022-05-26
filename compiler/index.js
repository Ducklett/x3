const { inspect } = require('util')
const { read } = require('./util')
const { parse } = require('./parser')
const { bind } = require('./binder')
const { lower } = require('./lowerer')
const { emitAsm } = require('./emitter')
const { outputHtml } = require('./tool/outputHtml')
const { hasErrors: hasErrors, displayErrors } = require('./errors')

// compile('examples/09_rtti.x3')
compile('examples/10_snake.x3')
// compile('examples/11_enum.x3')
// compile('examples/coreutils/test.x3')

function compile(filename) {
	const file = ({ path: filename, code: read(filename) })
	const syntaxTree = parse(file)
	if (hasErrors()) displayErrors()

	const ast = bind(syntaxTree)
	if (hasErrors()) displayErrors()

	const [loweredAst, meta] = lower(ast)
	const entrypoint = meta.entrypoint?.name || 'main'
	emitAsm(loweredAst, { entrypoint })

	// outputHtml(syntaxTree)

	// console.log(inspect(ast, { depth: 2 }))
}
