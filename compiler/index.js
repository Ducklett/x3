const { parse } = require("./parser")
const { outputHtml } = require("./tool/outputHtml")

const code = `
((see linux manual for syscall documentation))
#syscall(0x001) proc write[fd:int,buf:*void,bufLen:int];
#syscall(0x03c) proc exit[code:int];

const stdin:int = 0;
const stdout:int = 1;
const stderr:int = 2;

#entrypoint
proc main[] {
    const msg = "hello world";
    write(stdin,msg,msg.length);
    exit(0);
}
`
const st = parse(code)

outputHtml(st)
