/*
temporary script to test the emitter
imports helper functions that let you create the AST
and then feeds that to the emitter to output assembly

this will be obselete once we have a parser,
but the AST creation API might still be used for unit tests, user-facing compiler plugins, etc.
*/
const { assert, MARK, emitAsm, syscall, fn, If, param, call, num, binary, readVar, readProp, declareVar, assignVar, fd, unary, ret, str } = require('./compiler')

let message
let message2 = 'hello ';
let message3 = '\n';
let messageLong = 'You have a long name\n';
let messageShort = 'Please enter a name\n';
let inputbuffer = ''.padStart(40);
let main, fadd, nameLen, exitCode, a, b;
const ast = [
	message = MARK('const')(declareVar('message', str('what\'s your name?\n'))),

	fadd = MARK("doesNotCallOtherFunctions", "doesNotAllocate")
		(fn('add', [a = param('a'), b = param('b')], [
			ret(binary('+', readVar(a), readVar(b)))
		])),
	MARK("doesNotCallOtherFunctions", "doesNotAllocate")
		(fn('retard', [], [ret(num(420))])),
	main = fn('main', [], [
		nameLen = declareVar('nameLen'),
		exitCode = declareVar('exitCode'),

		syscall('write', num(fd.stdout), readVar(message), readProp(message, 'length')),

		// TODO: make the other strings constants too
		assignVar(nameLen, syscall('read', num(fd.stdin), str(inputbuffer), num(inputbuffer.length))),
		unary('pre--', readVar(nameLen)),

		If(binary('<=', readVar(nameLen), num(1)), [
			syscall('write', num(fd.stdout), str(messageShort), num(messageShort.length)),
			syscall('exit', num(1)),
		]),

		syscall('write', num(fd.stdout), str(message2), num(message2.length)),
		syscall('write', num(fd.stdout), str(inputbuffer), readVar(nameLen)),
		syscall('write', num(fd.stdout), str(message3), num(message3.length)),

		If(binary('>', readVar(nameLen), num(10)), [
			syscall('write', num(fd.stdout), str(messageLong), num(messageLong.length)),
		]),

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
