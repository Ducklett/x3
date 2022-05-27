const { assert } = require("./util")

const tag_error = -1
const tag_void = 0
const tag_int = 1
const tag_bool = 2
const tag_string = 3
const tag_char = 4
const tag_pointer = 5
const tag_array = 6
const tag_struct = 7
const tag_type = 8
const tag_enum = 9
const tag_function = 10
const tag_float = 11
const tag_buffer = 12

const typeMap = {
	'error': { tag: tag_error, type: 'error', size: -1 },
	'unknown': { tag: tag_void, type: 'unknown', size: 0 },
	'null': { tag: tag_pointer, type: 'null', size: 0 },
	'int': { tag: tag_int, type: 'int', size: 8, signed: true },
	'uint': { tag: tag_int, type: 'uint', size: 8, signed: false },
	'u64': { tag: tag_int, type: 'u64', size: 8, signed: false },
	's64': { tag: tag_int, type: 's64', size: 8, signed: true },
	'u32': { tag: tag_int, type: 'u32', size: 4, signed: false },
	's32': { tag: tag_int, type: 's32', size: 4, signed: true },
	'u16': { tag: tag_int, type: 'u16', size: 2, signed: false },
	's16': { tag: tag_int, type: 's16', size: 2, signed: true },
	'u8': { tag: tag_int, type: 'u8', size: 1, signed: false },
	's8': { tag: tag_int, type: 's8', size: 1, signed: true },
	'f64': { tag: tag_float, type: 'f64', size: 8, signed: true },
	'f32': { tag: tag_float, type: 'f32', size: 4, signed: true },
	'void': { tag: tag_void, type: 'void', size: 0 },
	'string': { tag: tag_string, type: 'string', size: 16 },  // *char, length
	'cstring': { tag: tag_pointer, type: 'cstring', size: 8 }, // *char
	'array': { tag: tag_array, type: 'array', size: 16 },    // *values,length
	'buffer': { tag: tag_buffer, type: 'buffer', size: 0 },   // contiguous block of memory
	'char': { tag: tag_char, type: 'char', size: 1 },
	'bool': { tag: tag_bool, type: 'bool', size: 1 },
	'pointer': { tag: tag_pointer, type: 'pointer', size: 8, to: undefined },
	'function': { tag: tag_function, type: 'function', size: 8, params: [], returns: undefined },
}

function cloneType(t) {
	return { ...t }
}

function typeInfoLabel(type) {
	const l = []
	l.push(type.type)
	l.push('$typeinfo')

	if (type.kind == 'struct' || type.kind == 'union') {
	}

	if (type.type == 'pointer') {
		l.push('$')
		l.push(typeInfoLabel(type.to))
	}

	if (type.type == 'buffer') {
		if (type.size == 0) throw 'uninitialized buffer type'
		l.push('$')
		l.push(type.count)
		l.push('$')
		l.push(typeInfoLabel(type.of))
	}

	if (type.type == 'array') {
		l.push('$')
		l.push(typeInfoLabel(type.of))
	}

	return l.join('')
}

// rounds number op to the next factor of increment
// used for struct alignment
function roundToIncrement(number, increment) { return Math.ceil(number / increment) * increment }
function alignmentForSize(size) {
	if (size <= 0) throw 'illegal size'
	switch (size) {
		case 1: return 1
		case 2: return 2
		case 3: return 4
		case 4: return 4
		default: return 8
	}
}

function alignStructFields(fields, startOffset = 0) {
	let size = startOffset
	for (let field of fields) {
		assert(field.type)
		assert(field.type.size)

		// potentially fix alignment
		const alignment = alignmentForSize(field.type.size)
		size = roundToIncrement(size, alignment)

		if (field.offset === undefined) {
			field.offset = size
			size += field.type.size
		} else {
			size = Math.max(size, field.offset + field.type.size)
		}
	}

	const alignment = alignmentForSize(size)
	return roundToIncrement(size, alignment)
}

function alignUnionFields(fields) {
	let size = 0
	for (let field of fields) {
		assert(field.type)
		assert(field.type.size)
		field.kind = 'field'
		field.offset = 0
		size = Math.max(size, field.type.size)
	}

	const alignment = alignmentForSize(size)
	return roundToIncrement(size, alignment)
}

// compiler boilerplate injector
function B(node) {
	if (!node.span) node.span = { file: '<compiler>', from: 0, to: 0 }
	if (!node.notes) node.notes = new Map()
	return node
}

const nop = () => B({ kind: 'nop' })
const MARK = (...notes) => node => ({ ...node, notes: new Set(notes) })
const fn = (name, params, returnType, instructions, type) => B({
	kind: 'function',
	name,
	params,
	returnType,
	instructions,
	type
})
const struct = (name, fields) => {
	const scope = {
		name,
		kind: 'struct',
		parent: null,
		symbols: new Map(),
		fields,
		used: new Set(),
	}

	for (let f of fields) {
		scope.symbols.set(f.name, f)
	}

	const size = alignStructFields(fields)
	// const size = fields.reduce((acc, cur) => {
	// 	assert(cur.type.size > 0)
	// 	if (cur.offset === undefined) {
	// 		cur.offset = acc
	// 		acc += cur.type.size
	// 	} else {
	// 		acc = Math.max(acc, cur.type.size + cur.offset)
	// 	}
	// 	return acc
	// }, 0)
	return B({ tag: 7, kind: 'struct', name, type: name, fields, size, scope })
}

const union = (name, fields) => {
	const scope = {
		name,
		kind: 'union',
		parent: null,
		symbols: new Map(),
		used: new Set(),
	}

	for (let f of fields) {
		scope.symbols.set(f.name, f)
	}

	const size = fields.reduce((acc, cur) => {
		cur.offset = 0
		acc = Math.max(cur.type.size, acc)
		return acc
	}, 0)

	assert(size)
	assert(size % 8 == 0)

	return B({ kind: 'union', name, type: name, fields, size, scope })
}
const If = (cond, then, els = null) => B({ kind: 'if', cond, then, els })
const call = (def, ...args) => B({ kind: 'call', def, args })
const ctor = (def, ...args) => B({ kind: 'ctorcall', def, type: def, args })
const syscall = (code, ...args) => B({ kind: 'syscall', code, args })
const param = (name, type) => B({ kind: 'parameter', name, type })
const declareVar = (name, expr) => B({ kind: 'declareVar', name, expr, type: expr.type })
const assignVar = (varDec, expr) => B({ kind: 'assignVar', varDec, expr })
const ref = symbol => B({ kind: 'reference', symbol, type: symbol.type })
const readProp = (left, prop) => B({ kind: 'readProp', left, prop, type: prop.type })
const indexedAccess = (left, index) => {
	let type
	if (left.type.type == 'array' || left.type.type == 'buffer') type = left.type.of
	else if (left.type.type == 'string') type = cloneType(typeMap.char)
	else throw 'illegal type of offset access'

	const indirect = left.type.type != 'buffer'

	return B({
		kind: 'indexedAccess', left, index, type, indirect
	})
}
const binary = (op, a, b, type) => B({ kind: 'binary', op, a, b, type: type ?? a.type })
const unary = (op, expr, type) => B({ kind: 'unary', op, expr, type })
const ret = expr => B({ kind: 'return', expr })
const goto = (label, condition) => B({ kind: 'goto', condition, label })
const label = (name) => B({ kind: 'label', name })
const bool = (value, type) => B({ kind: 'booleanLiteral', value, type })
const num = (n, type) => {
	if (!type) throw 'num needs type'
	return B({ kind: 'numberLiteral', n, type })
}
const str = (value, type) => {
	const v = new TextEncoder().encode(value)
	return B({ kind: 'stringLiteral', value, len: v.length, type })
}

module.exports = {
	tag_error, tag_void, tag_int, tag_bool, tag_string, tag_char, tag_pointer, tag_array, tag_struct, tag_type, tag_enum, tag_function, tag_float, tag_buffer,
	typeMap,
	cloneType,
	typeInfoLabel,
	roundToIncrement,
	alignStructFields,
	alignUnionFields,

	nop,
	MARK,
	fn,
	struct,
	union,
	If,
	call,
	ctor,
	syscall,
	param,
	declareVar,
	assignVar,
	ref,
	readProp,
	indexedAccess,
	binary,
	unary,
	ret,
	goto,
	label,
	bool,
	num,
	str,
}
