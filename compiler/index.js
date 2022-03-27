const { parse } = require('./parser')
const { bind, lower } = require('./bind')
const { inspect } = require('util')
const { emitAsm, read } = require('./compiler')
const { outputHtml } = require('./tool/outputHtml')

compile('examples/sys.x3', 'examples/test.x3')

function compile(...filenames) {
    const files = filenames.map(read)
    const syntaxTree = parse(files)
    let ast = bind(syntaxTree)
    ast = lower(ast)

    //outputHtml(st)
    emitAsm(ast)

    //console.log(inspect(ast, { depth: 10 }))
}
