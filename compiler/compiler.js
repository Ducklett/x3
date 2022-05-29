let options

function newOptions() {
	return {
		includeObjs: [],
		includeLibs: [],
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
	const syntaxTree = parse(file)
	if (hasErrors()) displayErrors()

	const ast = bind(syntaxTree)
	if (hasErrors()) displayErrors()

	if (getFlag('noemit')) return

	const [loweredAst, meta] = lower(ast)
	const entrypoint = meta.entrypoint?.name || 'main'
	emitAsm(loweredAst, { entrypoint })

	// outputHtml(syntaxTree)

	const outObj = 'out/out.o'
	const outExecutable = 'out/out'
	includeObjInCompilation(outObj)

	const objs = options.includeObjs.join(' ')
	// console.log(inspect(ast, { depth: 2 }))
	exec(`mkdir -p out`)
	exec(`nasm -f elf64 out/out.asm -o ${outObj}`)
	exec(`ld ${objs} -o ${outExecutable}`)

	if (getFlag('run')) {
		exec(`./${outExecutable} these are some args`)
	}
}

module.exports = {
	includeObjInCompilation,
	includeLibInCompilation,
	getFlag,
	setFlag,
	compile,
}
