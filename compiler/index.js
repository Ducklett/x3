const { assert, emitAsm, syscall, fn, param, call, num, binary, readVar, declareVar, assignVar, fd, unary } = require('./compiler')

const message = 'what\'s your name?\n';
const message2 = 'hello ';
const message3 = '\n';
const inputbuffer = ''.padStart(40);
let main, fadd, nameLen, exitCode, a, b;
const ast = [
    fadd = fn('add', [a = param('a'), b = param('b')], [
        binary('+', readVar(a), readVar(b))
    ]),
    main = fn('main', [], [
        nameLen = declareVar('nameLen'),
        exitCode = declareVar('exitCode'),
        syscall('write', fd.stdout, message, message.length),
        assignVar(nameLen, syscall('read', fd.stdin, inputbuffer, inputbuffer.length)),
        syscall('write', fd.stdout, message2, message2.length),
        syscall('write', fd.stdout, inputbuffer, readVar(nameLen)),
        syscall('write', fd.stdout, message3, message3.length),
        assignVar(
            exitCode,
            // 36 % (3+7) = exit(6)
            binary('%',
                num(36),
                call(fadd, num(3), num(7))
            )
        ),
        unary('pre++', readVar(exitCode)),
        syscall('exit', readVar(exitCode)),
    ]),
];
emitAsm(ast, 'out/out.asm');
