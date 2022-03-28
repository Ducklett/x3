const { parse } = require('./parser')
const { bind, lower } = require('./bind')
const { inspect } = require('util')
const { emitAsm, read } = require('./compiler')
const { outputHtml } = require('./tool/outputHtml')

compile('examples/02_read_stdin.x3')

function compile(filename) {
    const file = ({ path: filename, code: read(filename) })
    const syntaxTree = parse(file)
    let ast = bind(syntaxTree)
    ast = lower(ast)

    //outputHtml(st)
    emitAsm(ast)

    // console.log(inspect(ast, { depth: 2 }))
}
