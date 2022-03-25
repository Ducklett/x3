const { parse } = require("./parser")
const { bind } = require("./bind")
const { inspect } = require("util")
const { emitAsm } = require("./compiler")

const code = `
((see linux manual for syscall documentation))
#syscall(0x001) proc write[fd:int,buf:*void,bufLen:int];
#syscall(0x03c) proc exit[code:int];

const stdin:int = 0;
const stdout:int = 1;
const stderr:int = 2;

((TODO: move this to function scope))
const msg = "hello world!\n";

#entrypoint
proc main[] {
    write(stdout,msg,msg.length);
    exit(0);
}
`
const st = parse(code)
const ast = bind(st)

emitAsm(ast)

//console.log(inspect(ast, { depth: 10 }))
