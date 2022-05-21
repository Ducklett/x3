const path = require('path')
const fs = require('fs')

function read(filename) {
	return fs.readFileSync(filename, 'utf-8')
}

function write(filename, content) {
	return fs.writeFileSync(filename, content)
}

function assert(expr, msg = '') {
	if (expr) return
	const stack = new Error().stack
	const failedAt = stack.split('\n')[2]
	const matched =
		failedAt.match(/\((.*):(\d+:\d+)\)/) ?? failedAt.match(/at (.*):(\d+:\d+)/)
	if (matched === null) {
		console.error('stack parsing error in assert, update it!')
		process.exit(1)
	}
	const [_, file, line] = matched
	const relativeToRoot = path.relative(process.cwd(), file)
	const displayMessage = msg ? `: '${msg}'` : ''
	console.log(`${relativeToRoot}:${line} assertion failed${displayMessage}`)
	process.exit(0)
}

function spanFromRange(from, to) {
	return { ...from, to: to.to }
}

function escapeString(c) {
	return c.replace(/\n/g, '\\n')
		.replace(/\0/g, '\\0')
}

function unEscapeString(str) {
	return str.replace(/\\n/g, '\n')
		.replace(/\\r/g, '\r')
		.replace(/\\t/g, '\t')
		.replace(/\\0/g, '\0')
		.replace(/\\"/g, '"')
}

module.exports = { read, write, assert, spanFromRange, escapeString, unEscapeString }
