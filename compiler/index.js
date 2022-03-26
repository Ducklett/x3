const { parse } = require("./parser")
const { bind } = require("./bind")
const { inspect } = require("util")
const { emitAsm, read } = require("./compiler")
const { outputHtml } = require("./tool/outputHtml")

const code = read('test.x3')

const st = parse(code)
const ast = bind(st)

outputHtml(st)
emitAsm(ast)

//console.log(inspect(ast, { depth: 10 }))
