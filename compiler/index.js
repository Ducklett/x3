const { parse } = require('./parser')
const { bind, lower } = require('./bind')
const { inspect } = require('util')
const { emitAsm, read } = require('./compiler')
const { outputHtml } = require('./tool/outputHtml')

compile('examples/09_rtti.x3')

function compile(filename) {
    const file = ({ path: filename, code: read(filename) })
    const syntaxTree = parse(file)
    const ast = bind(syntaxTree)
    const [loweredAst, meta] = lower(ast)

    const entrypoint = meta.entrypoint?.name || 'main'

    //outputHtml(st)
    emitAsm(loweredAst, { entrypoint })

    // console.log(inspect(ast, { depth: 2 }))
}
