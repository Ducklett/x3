const { assert, emitAsm, syscall, fn, call, num, add, readVar, declareVar, assignVar, fd } = require('./compiler')

const message = 'what\'s your name?\n';
const message2 = 'hello ';
const message3 = '\n';
const inputbuffer = ''.padStart(40);
let nameLen, exitCode, a, b;
const ast = [
    fn('main', [
        nameLen = declareVar('nameLen'),
        syscall('write', fd.stdout, message, message.length),
        assignVar(nameLen, syscall('read', fd.stdin, inputbuffer, inputbuffer.length)),
        syscall('write', fd.stdout, message2, message2.length),
        syscall('write', fd.stdout, inputbuffer, readVar(nameLen)),
        syscall('write', fd.stdout, message3, message3.length),
        exitCode = declareVar('exitCode'),
        assignVar(exitCode, call('add')),
        syscall('exit', readVar(exitCode))
    ]),
    fn('add', [
        a = declareVar('a'),
        b = declareVar('b'),
        assignVar(a, num(10)),
        assignVar(b, num(20)),
        add(readVar(a), readVar(b))
    ])
];
emitAsm(ast, 'out/out.asm');
