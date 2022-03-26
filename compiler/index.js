const { parse } = require('./parser')
const { bind, lower } = require('./bind')
const { inspect } = require('util')
const { emitAsm, read } = require('./compiler')
const { outputHtml } = require('./tool/outputHtml')

const code = read('test.x3')

const st = parse(code)
let ast = bind(st)
ast = lower(ast)

//outputHtml(st)
emitAsm(ast)

//console.log(inspect(ast, { depth: 10 }))
