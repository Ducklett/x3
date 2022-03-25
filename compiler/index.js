const { parse } = require("./parser")
const { bind } = require("./bind")
const { inspect } = require("util")
const { emitAsm } = require("./compiler")
const { outputHtml } = require("./tool/outputHtml")

const code = `
((see linux manual for syscall documentation))
#syscall(0x000) proc read[fd:int,buf:*void,count:int] -> int;
#syscall(0x001) proc write[fd:int,buf:*void,count:int];
#syscall(0x03c) proc exit[code:int];

const stdin:int = 0;
const stdout:int = 1;
const stderr:int = 2;

((TODO: move this to function scope))
const msg = "What's your name?\n";
const greet = "hello ";
const end = "!\n";
const name = "                        ";

#entrypoint
proc main[] {
    var len:int

    write(stdout,msg,msg.length);

    len = read(stdin, name, name.length);

    write(stdout,greet,greet.length);
    write(stdout,name,len-1);
    write(stdout,end,end.length);
    exit(0);
}
`
const st = parse(code)
const ast = bind(st)

outputHtml(st)
emitAsm(ast)

//console.log(inspect(ast, { depth: 10 }))
