const { assert, emitAsm, syscall, fn, param, call, num, add, readVar, declareVar, assignVar, fd } = require('./compiler')

const message = 'what\'s your name?\n';
const message2 = 'hello ';
const message3 = '\n';
const inputbuffer = ''.padStart(40);
let main, fadd, nameLen, exitCode, a, b;
const ast = [
    fadd = fn('add', [a = param('a'), b = param('b')], [
        add(readVar(a), readVar(b))
    ]),
    main = fn('main', [], [
        nameLen = declareVar('nameLen'),
        syscall('write', fd.stdout, message, message.length),
        assignVar(nameLen, syscall('read', fd.stdin, inputbuffer, inputbuffer.length)),
        syscall('write', fd.stdout, message2, message2.length),
        syscall('write', fd.stdout, inputbuffer, readVar(nameLen)),
        syscall('write', fd.stdout, message3, message3.length),
        exitCode = declareVar('exitCode'),
        assignVar(exitCode, call(fadd, num(10), num(20))),
        syscall('exit', readVar(exitCode)),
    ]),
];
emitAsm(ast, 'out/out.asm');
