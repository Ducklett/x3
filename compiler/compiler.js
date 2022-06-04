let options

function newOptions() {
	return {
		includeObjs: [],
		includeLibs: [],
		includeLibPaths: [],
		flags: {
			run: true,
			boring: false,
		},
	}
}

function getFlag(k) { return options.flags[k] }
function setFlag(k, v) { return options.flags[k] = v }

function includeObjInCompilation(path) {
	options.includeObjs.push(path)
}

function includeLibInCompilation(path) {
	options.includeLibs.push(path)
}

function includeLibPathInCompilation(path) {
	options.includeLibPaths.push(path)
}

function compile(filename) {
	const { inspect } = require('util')
	const { read, assert } = require('./util')
	const { parse } = require('./parser')
	const { bind } = require('./binder')
	const { lower } = require('./lowerer')
	const { emitAsm } = require('./emitter')
	const { outputHtml } = require('./tool/outputHtml')
	const { hasErrors: hasErrors, displayErrors } = require('./errors')
	const { execSync } = require('child_process')

	function exec(cmd) {
		try {
			if (process.platform == 'win32') {
				cmd = cmd.replace(/\//g, '\\')
			}

			console.log(cmd)
			execSync(cmd, { stdio: 'inherit' });
		} catch (e) {
			process.exit(e.status)
		}
	}

	options = newOptions()

	const args = process.argv.slice(2)
	for (let arg of args) {
		if (!arg.startsWith('--')) break
		switch (arg) {
			case '--boring': setFlag('boring', true); break
			case '--run': setFlag('run', true); break
			// opposite of --boring
			case '--human': setFlag('boring', false); break
			// opposite of --run
			case '--compile': setFlag('run', false); break
			case '--typecheck':
				setFlag('boring', true)
				setFlag('noemit', true)
				break
			default: throw `unsupported arg ${arg}`
		}
	}
	const file = ({ path: filename, code: read(filename) })
	const syntax = parse(file)

	const ast = bind([syntax])
	if (hasErrors()) displayErrors()

	if (getFlag('noemit')) return

	const [loweredAst, meta] = lower(ast)
	const entrypoint = meta.entrypoint?.name || 'main'
	emitAsm(loweredAst, { entrypoint })

	// outputHtml(syntaxTree)

	const outObj = 'out/out.o'
	const outExecutable = process.platform == 'win32'
		? 'out/out.exe'
		: 'out/out'

	includeObjInCompilation(outObj)

	const objs = options.includeObjs.join(' ')
	const libPaths = options.includeLibPaths.map(l => `-L${l}`).join(' ')
	const libs = options.includeLibs.map(l => `-l${l}`).join(' ')
	const linkerCommand = `ld -e _start ${objs} ${libPaths} ${libs}  -o ${outExecutable}`
	// console.log(inspect(ast, { depth: 2 }))
	// exec(`mkdir -p out`)
	exec(`nasm -f elf64 out/out.asm -o ${outObj}`)
	exec(linkerCommand)

	if (getFlag('run')) {
		exec(`./${outExecutable} these are some args`)
	}
}

module.exports = {
	includeObjInCompilation,
	includeLibInCompilation,
	includeLibPathInCompilation,
	getFlag,
	setFlag,
	compile,
}
