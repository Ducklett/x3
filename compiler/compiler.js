const path = require('path')
const fs = require('fs')

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

// return double as array of two u32 values
function f64ToBytes(f) {
	const buf = new Float64Array(1)
	const a = new Uint32Array(buf.buffer)
	buf[0] = f
	return [a[0], a[1]]
}

// return float as u32
function f32ToBytes(f) {
	const buf = new Float32Array(1)
	const a = new Uint32Array(buf.buffer)
	buf[0] = f
	return a[0]
}

function escapeCharSequence(c) {
	return c.replace(/\n/g, '\\n')
		.replace(/\0/g, '\\0')
}
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

// compiler boilerplate injector
function B(node) {
	if (!node.span) node.span = { file: '<compiler>', from: 0, to: 0 }
	if (!node.notes) node.notes = new Map()
	return node
}

const api = {
	read,
	write,
	assert,
	roundToIncrement(number, increment) { return Math.ceil(number / increment) * increment },

	fd: {
		stdin: 0,
		stdout: 1,
		stderr: 2
	},

	nop: () => B({ kind: 'nop' }),
	MARK: (...notes) => node => ({ ...node, notes: new Set(notes) }),
	fn: (name, params, returnType, instructions, type) => B({
		kind: 'function',
		name,
		params,
		returnType,
		instructions,
		type
	}),
	struct: (name, fields) => {
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

		const size = fields.reduce((acc, cur) => {
			assert(cur.type.size > 0)
			if (cur.offset === undefined) {
				cur.offset = acc
				acc += cur.type.size
			} else {
				acc = Math.max(acc, cur.type.size + cur.offset)
			}
			return acc
		}, 0)
		return B({ tag: 7, kind: 'struct', name, type: name, fields, size, scope })
	},
	union: (name, fields) => {
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
	},
	If: (cond, then, els = null) => B({ kind: 'if', cond, then, els }),
	call: (def, ...args) => B({ kind: 'call', def, args }),
	ctor: (def, ...args) => B({ kind: 'ctorcall', def, type: def, args }),
	syscall: (code, ...args) => B({ kind: 'syscall', code, args }),
	param: (name, type) => B({ kind: 'parameter', name, type }),
	declareVar: (name, expr) => B({ kind: 'declareVar', name, expr, type: expr.type }),
	assignVar: (varDec, expr) => B({ kind: 'assignVar', varDec, expr }),
	ref: symbol => B({ kind: 'reference', symbol, type: symbol.type }),
	readProp: (left, prop) => B({ kind: 'readProp', left, prop, type: prop.type }),
	indexedAccess: (left, index) => B({
		kind: 'indexedAccess', left, index, type: (() => {
			if (left.type.type == 'array') return left.type.of
			if (left.type.type == 'string') return { type: 'char', size: 1 }
			throw 'illegal type of offset access'
		})()
	}),
	binary: (op, a, b, type) => B({ kind: 'binary', op, a, b, type: type ?? a.type }),
	unary: (op, expr, type) => B({ kind: 'unary', op, expr, type }),
	ret: expr => B({ kind: 'return', expr }),
	goto: (label, condition) => B({ kind: 'goto', condition, label }),
	label: (name) => B({ kind: 'label', name }),

	bool: (v, type) => B({ kind: 'booleanLiteral', v, type }),
	num: (n, type) => {
		if (!type) throw 'num needs type'
		return B({ kind: 'numberLiteral', n, type })
	},
	str: (value, type) => {
		const v = new TextEncoder().encode(value)
		return B({ kind: 'stringLiteral', value, len: v.length, type })
	},

	emitAsm(ast, { dest = 'out/out.asm', entrypoint = 'main' } = {}) {
		let locals

		// variable => label lookup
		const globals = new Map()

		// data => label lookup
		const datalut = new Map()
		// label => data lookup
		const data = new Map()

		const label = (content = null) => {
			if (!global.labelIndex) global.labelIndex = 0

			if (content !== null) {
				const foundL = datalut.get(content)
				if (foundL) return foundL
				global.labelIndex++
				const l = `__label${global.labelIndex}`
				data.set(l, content)
				datalut.set(content, l)
				return l
			} else {
				global.labelIndex++
				const l = `__label${global.labelIndex}`
				return l
			}
		}

		const lines = []
		let hasEntrypoint = false
		let returnLabel = null

		// ==================

		// HACK: variables first
		for (let node of ast) {
			if (node.kind == 'declareVar') emitTop(node)
		}

		for (let node of ast) {
			if (node.kind != 'declareVar') emitTop(node)
		}

		assert(hasEntrypoint, 'the program defines a main function')
		const x3rt0 = `
; x3rt0
_start:
xor rbp, rbp

; call main(args: []cstring)
lea rax, [rsp+8]    ; load argv first, because push will move rsp
push qword [rsp]    ; argc
push rax            ; argv
call _main

; call exit(0)
mov rax, 0x3c
mov rdi, 0
syscall`

		const source = `
section .text
global _start
${entrypoint == 'main' ? x3rt0 : ''}
${lines.join('\n')}
section .data
${[...data.keys()]
				.map(k => {
					let d = data.get(k)
					if (typeof d === 'number') {
						return `${k} equ ${d}`
					}

					if (typeof d === 'string') {
						const bytes = [...d].map(c => c.charCodeAt(0)).join(',')
						return `${k} db ${bytes} ; '${d.slice(0, 20).replace(/(\n|\r|\0)/g, '')}'`
					}

					if (!Array.isArray(d)) {
						console.log(d)
					}

					assert(Array.isArray(d))
					return `${k} dq ${d.join(', ')}`
				})
				.join('\n')}
`
		write(dest, source)

		// ==================

		function emitArgs(args) {
			// NOTE: we push the last item first, because the stack grows down
			// TODO: evaluate the args from left to right while still keeping the proper stack position
			// NOTE: currently the last arg is evaluated first, which may lead to some unexpected stuff happening if it mutates state
			for (let arg of [...args].reverse()) {
				emitExpr(arg)
			}
		}

		function emitVar(node, fieldOffset = 0) {
			const legalReferenceKinds = new Set(['declareVar', 'parameter', 'stringLiteral', 'arrayLiteral', 'numberLiteral', 'function'])

			// if (!(typeof node == 'string' || legalReferenceKinds.has(node.kind))) throw 'aaa'

			assert(typeof node == 'string' || legalReferenceKinds.has(node.kind), `expected declareVar, got ${node.kind}`)

			if (node.kind == 'function') {
				return '_' + node.name
			}

			const registers = new Set("rax,rcx,rdx,rbx,rsi,rdi,rsp,rbp,r8,r9,r10,r11,r12,r13,r14,r15".split(','))
			if (registers.has(node)) {
				return `[${node} + ${fieldOffset}]`
			}

			let offset = locals.get(node)
			if (offset) {
				assert(offset !== undefined)
				if (fieldOffset) {
					assert(typeof offset == 'number')
					offset += fieldOffset
				}
				if (offset == 0) return '[rbp]'
				return `[rbp${offset > 0 ? '+' : ''}${offset}]`
			} else {
				// NOTE: globals always store *labels* to data
				// so they are pointers
				// to return the actual data we should do [offset]
				offset = globals.get(node)
				if (offset) {
					if (fieldOffset) {
						if (typeof offset == 'number') {
							offset += fieldOffset
						} else {
							offset += ' + ' + fieldOffset
						}
					}

					if (node.type.name == 'type info') {
						// HACK: treat type info as a pointer
					} else {
						offset = `[${offset}]`
					}
				}

				if (offset === undefined) {
					console.log('var not found!')
					console.log(node)
				}
				assert(
					offset !== undefined,
					'referenced variable will either be a local or a global'
				)
				return offset
			}
		}

		// NOTE: data is currently expected to be an array of qwords, other types won't work!
		// create labels for referenced data and return the data
		function emitStatic(node, data = []) {
			switch (node?.kind) {
				case 'stringLiteral': {
					const l = label(node.value)
					data.push(l)
					data.push(node.len)
				} break
				case 'numberLiteral': {
					assert(node.type.tag == tag_int)
					data.push(node.n)
				} break
				case 'booleanLiteral': {
					// NOTE: padded to qword...
					data.push(node.v ? 1 : 0)
				} break
				case 'declareVar': {
					// this acts as a pointer to a variable..

					assert(node.expr)
					const l = globals.get(node)
					assert(l)
					data.push(l)
				} break

				case 'arrayLiteral': {
					const arrayData = []
					for (let entry of node.entries) {
						// assert(entry.kind == 'stringLiteral')
						emitStatic(entry, arrayData)
					}
					const l = label(arrayData)
					// HACK: put label on expr so we can reference it there
					node.l = l
					data.push(l)
					data.push(node.entries.length)
				} break
				case 'ctorcall': {
					// NOTE: this only works for structs, not unions

					for (let field of node.args) {
						emitStatic(field, data)
					}
				} break
				default:
					console.log(data)
					throw 'unhandled case'
					break
			}
			return data
		}

		function emitTop(node) {
			switch (node.kind) {
				case 'declareVar': {
					const notes = node.notes ?? new Set()
					assert(notes.has('const'), 'top level variables are constant')
					if (!node.expr) {
						console.log(node)
					}
					assert(node.expr, 'top level variable is intialized')
					const data = emitStatic(node.expr)

					// create a label for this data
					// then we can do [l] for the data or l for the pointer

					const l = label(data)
					globals.set(node, l)

					return
				}
				case 'function': {
					// extern declaration; don't emit
					if (node.notes.has('syscall')) return

					assert(node.instructions, `functions have a body`)

					let optimizations = node.notes ?? new Set()
					let removeAlloc = optimizations.has('doesNotAllocate')

					returnLabel = label()
					locals = new Map()
					let name = node.name
					const isEntrypoint = name === entrypoint
					if (isEntrypoint) {
						assert(!hasEntrypoint, `only has one entrypoint`)
						hasEntrypoint = true
						// custom entrypoints skip x3rt0
						if (name != 'main') lines.push(`_start:`)
					}

					const vars = node.instructions.filter(n => n.kind === 'declareVar' || n.kind == 'buffer')
					const exprs = node.instructions.filter(n => n.kind !== 'declareVar' && n.kind != 'buffer')

					lines.push(`_${name}:`)

					let localSize = vars.reduce((acc, cur) => {
						if (cur.kind == 'buffer') {
							if (cur.data.kind == 'arrayLiteral') {
								assert(cur.data.type.count !== undefined)

								// TODO: also check if this one is correct? I think it should be cur.data.type.of.size
								const size = cur.data.type.count * cur.data.type.size
								if (size % 8 != 0) {
									console.log(cur)
								}
								assert(size % 8 == 0)
								return acc + size
							} else {
								if (cur.data.kind != 'stringLiteral') {
									console.log(cur)
								}
								assert(cur.data.kind == 'stringLiteral')
								const count = Math.ceil(cur.data.len / 8) * 8
								return acc + count
							}
						}
						assert(cur.type.size && cur.type.size > 0)

						return acc + Math.ceil(cur.type.size / 8) * 8 // alignment hack
					}, 0)

					lines.push(`; prologue`)
					lines.push(`push rbp`)
					lines.push(`mov rbp, rsp`)
					if (!removeAlloc) lines.push(`sub rsp, ${localSize}\n`)

					const passReturnByReference = node.returnType.size > 8

					const paramStartOffset = 16 + (passReturnByReference ? 8 : 0)
					let paramOffset = paramStartOffset + node.params.reduce((acc, cur) => {
						assert(cur.type.size && cur.type.size > 0)
						return acc + Math.ceil(cur.type.size / 8) * 8 // alignment hack
						// return acc + cur.type.size
					}, 0)

					for (let param of [...node.params].reverse()) {
						paramOffset -= Math.ceil(param.type.size / 8) * 8 // alignment hack
						locals.set(param, paramOffset)
						// console.log(`param ${param.name} = [rbp+${paramOffset}]`)
					}

					let varOffset = 0

					for (let vr of vars) {
						if (vr.kind == 'buffer') {
							const node = vr.data

							if (node.kind == 'numberLiteral') {
								assert(node.type.tag == tag_int)
								const size = node.type.size
								assert(size % 8 == 0)
								varOffset -= size

								// lines.push(`mov rax, ${node.n}`)
								// lines.push(`mov [rbp-${Math.abs(varOffset)}], rax`)
								lines.push(`mov qword [rbp-${Math.abs(varOffset)}], ${node.n}`)

								locals.set(node, varOffset)
							}
							else if (node.kind == 'stringLiteral') {
								const bufferLen = Math.ceil(node.len / 8) * 8
								var value = [...new TextEncoder().encode(node.value)]
								while (value.length < bufferLen) {
									value.push(0)
								}

								lines.push(`; ${escapeCharSequence(node.value)}`)
								for (let i = bufferLen - 8; i >= 0; i -= 8) {
									let chunk = 0n
									// it goes in reverse for some reason
									for (let j = 7; j >= 0; j--) {
										chunk <<= 8n
										chunk |= BigInt(value[i + j])
									}

									const outChunk = '0x' + chunk.toString(16)

									varOffset -= 8

									lines.push(`mov rax, ${outChunk}`)
									lines.push(`mov [rbp-${Math.abs(varOffset)}], rax`)
									// console.log(`var [rbp-${Math.abs(varOffset)}] = "${outChunk}"`)
								}
								locals.set(node, varOffset)
								// console.log(`string = ${emitVar(node)}`)
							} else {
								assert(node.kind == 'arrayLiteral')

								// TODO: check if this size calculation is correct? looks like i'm using type.size instead of type.of.size
								// NOTE: we will only initialize the members once we emit the array literal
								const size = node.type.count * node.type.size
								assert(size % 8 == 0)
								varOffset -= size
								locals.set(node, varOffset)
								// console.log(`array = ${emitVar(node)}`)
							}
						} else {
							if (vr.notes.has('const')) {
								//hoist that bitch!
								emitTop(vr)
								// console.log(`const ${vr.name} = ${emitVar(vr)}`)
							} else {
								assert(vr.kind == 'declareVar')
								assert(vr.expr == null, 'initalized locals not supported')
								varOffset -= Math.ceil(vr.type.size / 8) * 8 // alignment hack
								// varOffset -= vr.type.size
								locals.set(vr, varOffset)
								// console.log(`var ${vr.name} = ${emitVar(vr)}`)
							}
						}
					}

					lines.push(`; body`)
					for (let [expr, i] of exprs.map((v, i) => [v, i])) {
						emitExpr(expr, {
							shouldReturn: false,
							lastOfFunc: i == exprs.length - 1
						})
					}

					lines.push(`${returnLabel}:`)

					lines.push(`; epilogue`)
					lines.push(`mov rsp, rbp`)
					lines.push(`pop rbp`)
					lines.push('ret')
					return
				}
				default:
					assert(false, `unexpected top level kind ${node.kind}`)
			}
		}
		function emitExpr(node, { shouldReturn = true, lastOfFunc = false } = {}) {
			switch (node.kind) {
				case 'return': {
					// note that return WILL push the expr onto the stack
					if (node.expr) {
						if (node.expr.type.size <= 8) {
							emitExpr(node.expr)
							lines.push(`pop rax`)
						} else {
							assert(node.expr.type.size % 8 == 0)
							emitExpr(node.expr)
							lines.push(`mov rcx, [rbp+16]`)
							for (let i = 0; i < node.expr.type.size; i += 8) {
								lines.push(`pop rax`)
								lines.push(`mov [rcx+${i}], rax`)
							}
							lines.push(`mov rax, rcx`)
						}
					}
					lines.push(`; return`)
					lines.push(`jmp ${returnLabel}`)
					return
				}
				case 'syscall': {
					assert(node.code.kind == 'numberLiteral')
					assert(node.code.type.tag == tag_int)
					const nr = node.code.n
					assert(nr !== undefined, 'must be a linux syscall')
					// TODO: code->name mapping for bebug info
					lines.push(`; ${node.code.n} syscall`)
					const argRegisters = ['rdi', 'rsi', 'rdx', 'r10', 'r8', 'r9']
					for (let i = 0; i < node.args.length; i++) {
						const arg = node.args[i]
						assert(typeof arg === 'object', 'must be object')

						// TODO: get rid of this hack
						// evil casting hack
						let hackType = false
						if (arg.type.type == 'string') {
							hackType = true
							arg.type.type = 'cstring'
							arg.type.size = 8
						}
						emitExpr(arg)

						if (hackType) {
							arg.type.type = 'string'
							arg.type.size = 16
						}

						lines.push(`pop ${argRegisters[i]}`)
						// lines.push(`mov ${argRegisters[i]}, ${emitVar(arg.varDec)}`);
					}
					lines.push(`mov rax, ${nr}`)
					lines.push('syscall')
					if (shouldReturn) lines.push('push rax')
					lines.push('')
					return
				}
				case 'ctorcall': {
					lines.push(`; new ${node.type.name}()`)

					const args = node.args

					const targetSize = node.type.size

					if (targetSize == undefined) throw 'expected target size'

					// unions might need some padding since the constructors are of variable size
					// push the padding first since this means it will be added to the end of the object
					// let startByte = args.reduce((acc, cur) => Math.min(acc, cur.type.offset ?? 0), Number.POSITIVE_INFINITY)
					let size = args.reduce((acc, cur) => acc + cur.type.size, 0)
					assert(!isNaN(size))
					// assert(!isNaN(endByte))
					// assert(startByte != Number.POSITIVE_INFINITY)

					// if ((startByte != 0 || endByte != targetSize) && targetSize != 0) {
					//     console.log(`${startByte}<args>${endByte} :: ${targetSize}`)
					// }

					while (size < targetSize) {
						lines.push('push qword 0 ; end padding')
						size += 8
					}

					emitArgs(args)


					// while (startByte > 0) {
					//     lines.push('push qword 0 ; start padding')
					//     startByte -= 8
					// }

					return
				}
				case 'call': {
					const isLambda = node.def.symbol.kind != 'function'
					lines.push(
						`; ${node.def.symbol.name}(${isLambda ? '?' : node.def.symbol.params.map(n => n.name).join(', ')})`
					)
					const args = node.args ?? []
					let argSize = args.reduce((acc, cur, i) => {
						assert(cur.type, `has a type`)
						// TODO: switch to C-style calling convention so we can have 1-byte arguments
						const size = Math.ceil(cur.type.size / 8) * 8 // alignment hack
						if (size <= 0) console.log(cur)
						assert(size > 0, `type has size`)
						return acc + size
					}, 0)

					const returnSize = node.type.size
					const returnByReference = returnSize > 8

					if (returnByReference) {
						assert(returnSize % 8 == 0)
						lines.push(`sub rsp, ${returnSize} ; allocate returned struct`)
					}
					if (argSize) {
						emitArgs(args)
					}


					if (returnByReference) {
						// push return value address
						lines.push(`lea rax, [rsp+${argSize}] `)
						lines.push(`push rax`)
						argSize += 8
					}

					if (isLambda) {
						emitExpr(node.def)
						lines.push(`pop rax`)
						lines.push(`call rax`)
					} else {
						lines.push(`call _${node.def.symbol.name}`)
					}
					if (argSize) {
						lines.push(`add rsp, ${argSize}`)
					}
					if (shouldReturn) {
						if (returnByReference) {
							// NOTE: we already allocated the struct!
							// it was filled in by the call
							// we don't have to do anything else
						} else {
							lines.push(`push rax`)
						}
					}
					lines.push(``)
					return
				}
				case 'goto': {
					if (node.condition) {
						emitExpr(node.condition)
						lines.push(`pop rax`)
						lines.push(`cmp rax, 1`)
						lines.push(`je .${node.label.name}`)
					} else {
						lines.push(`jmp .${node.label.name}`)
					}

					return
				}
				case 'label': {
					lines.push(`.${node.name}:`)
					return
				}
				case 'reference': {
					assert(shouldReturn, 'reference should not be called at top level')
					// NOTE: reference should only be used for stack allocated values
					// for constants the literals should be statically allocated and inlined

					assert(node.type.size && node.type.size > 0)

					const size = node.type.size

					// char hack
					if (size == 1) {
						lines.push(`push qword ${emitVar(node.symbol)}`)
						return
					}

					assert(size % 8 == 0)

					lines.push(`; push ${node.symbol.name}`)

					for (let i = node.type.size - 8; i >= 0; i -= 8) {
						lines.push(`push qword ${emitVar(node.symbol, i)}`)
					}
					return
				}
				case 'indexedAccess': {
					assert(node.left.kind == 'reference')
					const legalTypes = ['array', 'pointer', 'string', 'cstring']
					assert(node.left.type.kind == 'struct' || legalTypes.includes(node.left.type.type))

					const elementType = node.type
					let elementSize = elementType.size
					assert(elementSize > 0)

					// TODO: allow values of less than 8 bytes on the stack
					// char hack
					if (elementSize == 1) {
						lines.push(`; ${node.left.symbol.name}[expr] CHAR`)

						// index on the stack
						emitExpr(node.index)
						lines.push(`pop r15`)

						// NOTE: we follow the pointer to return the value instead of its address
						// len
						lines.push(`mov rax, ${emitVar(node.left.symbol)}`)

						// TODO: figure out how to properly push just one byte
						lines.push(`mov rax, [rax+r15]`)
						lines.push(`and rax, 0xFF`)
						lines.push(`push qword rax`)
						return
					}

					assert(elementSize % 8 == 0)

					lines.push(`; ${node.left.symbol.name}[expr]`)

					// index on the stack
					emitExpr(node.index)
					lines.push(`pop r15`)
					lines.push(`imul r15, ${elementSize}`)

					// NOTE: we follow the pointer to return the value instead of its address
					// len
					lines.push(`mov rax, ${emitVar(node.left.symbol)}`)

					for (let i = elementSize - 8; i >= 0; i -= 8) {
						lines.push(`push qword [rax+r15+${i}]`)
					}
					return
				}
				case 'pad': {
					emitExpr(node.expr)
					assert(node.padding % 8 == 0)
					lines.push(`sub rsp, ${node.padding} ; padding`)
					return
				}
				case 'assignProp': {
					const vardec = node.left.symbol
					const prop = node.right.symbol
					const offset = prop.offset
					const size = prop.type.size
					assert(vardec.kind == 'declareVar')
					assert(offset !== undefined)
					assert(size > 0 && size % 8 == 0)

					emitExpr(node.expr)

					for (let i = 0; i < size; i += 8) {
						lines.push(`pop rax`)
						lines.push(`mov ${emitVar(vardec, offset + i)}, rax`)
					}

					return
				}
				case 'readProp': {
					if (node.left.kind == 'ctorcall') {
						const fieldIndex = node.left.type.fields.indexOf(node.prop.symbol)
						assert(fieldIndex != -1)
						const field = node.left.args[fieldIndex]
						emitExpr(field)
						return
					}
					// assert(node.left.kind == 'reference')
					assert(node.left.kind == 'reference' || node.left.kind == 'readProp')


					let prop = node.prop
					assert(prop.kind == 'reference')
					let offset = 0
					let left = node.left

					while (left.kind == 'readProp') {
						// console.log(left)
						// assert(false)

						assert(left.prop.type.type !== 'pointer')
						assert(left.prop.kind == 'reference')
						assert(left.prop.symbol.offset !== undefined)
						offset += left.prop.symbol.offset

						left = left.left
					}

					assert(left.kind == 'reference')

					const kind = left.symbol.kind
					left = left.symbol

					assert(kind == 'parameter' || kind == 'declareVar')

					assert(shouldReturn, 'reference should not be called at top level')

					// let offset = 0
					// let prop = node.prop

					// while (prop.kind == 'readProp') {
					//     assert(prop.left.kind == 'reference')
					//     assert(prop.left.symbol.offset !== undefined)
					//     offset += prop.left.symbol.offset

					//     prop = prop.prop
					// }

					// if (prop.kind != 'reference') {
					//     console.log(prop)
					// }
					assert(prop.kind == 'reference')
					assert(prop.symbol.offset !== undefined)

					offset += prop.symbol.offset

					const size = prop.symbol.type.size

					// let left = node.left.symbol

					// if (node.left.type.type == 'pointer') {
					if (left.type.type == 'pointer') {
						// lines.push(`mov rdx, ${emitVar(node.left.symbol)} ; (<~${left.name})`)
						lines.push(`mov rdx, ${emitVar(left)} ; (<~${left.name})`)
						left = 'rdx'
					}

					if (size == 1) {
						lines.push(`mov rax, ${emitVar(left, offset)} ; ${node.left.symbol.name}..${prop.symbol.name}`)
						lines.push(`and rax, 0xFF ; mask 1st byte`)
						lines.push(`push rax`)
					} else {
						assert(size && size % 8 == 0)
						for (let i = size - 8; i >= 0; i -= 8) {
							lines.push(`push qword ${emitVar(left, offset + i)} ; ${node.left.symbol.name}..${prop.symbol.name}`)
						}
					}
					return
				}
				case 'binary': {
					const ops = {
						'+': 'add',
						'-': 'sub',
						'*': 'imul',
						'/': 'idiv',
						'%': 'idiv',

						'>': 'setg',
						'>=': 'setge',
						'<': 'setl',
						'<=': 'setle',
						'==': 'sete',
						'!=': 'setne',
						'>>': 'sar', // TODO: research difference between shl/sal
						'<<': 'sal',
						'&': 'and',
						'|': 'or',
						'||': 'or',  // TODO: short circuit
						'&&': 'and', // TODO: short circuit
					}
					const op = ops[node.op]
					assert(op, `illegal binary operator ${node.op}`)

					let resultRegister = node.op == '%' ? 'rdx' : 'rax'

					emitExpr(node.a)
					emitExpr(node.b)

					if (node.op == '>>' || node.op == '<<') {
						lines.push(`pop rcx`)
						lines.push(`pop rax`)
						// variable shift uses cl register https://stackoverflow.com/questions/25644445/
						lines.push(`${op} rax, cl`)
						if (shouldReturn) lines.push(`push ${resultRegister}`)
						return
					}

					lines.push(`pop rcx`)
					lines.push(`pop rax`)
					if (op.startsWith('set')) {
						lines.push(`cmp rax, rcx`)
						// TODO: figure out if this is safe?
						// we save the result in al but push rax
						// so we're possibly saving garbage data from the upper bytes
						lines.push(`mov rax, 0`)
						lines.push(`${op} al`)
					} else if (op == 'idiv') {
						// as seen in C -> asm view on godbolt
						// cqo sign-extends rax:rdx
						// not really sure what this means, but it's needed to make *signed* division work
						// for unsigned division we'd use 'mul' over 'imul' and ditch 'cqo'
						// https://en.wikipedia.org/wiki/Sign_extension
						// https://stackoverflow.com/questions/36464879/when-and-why-do-we-sign-extend-and-use-cdq-with-mul-div
						// https://stackoverflow.com/questions/51717317/dividing-with-a-negative-number-gives-me-an-overflow-in-nasm/51717463#51717463
						lines.push(`cqo`)
						lines.push(`${op} rcx`)
					} else {
						lines.push(`${op} rax, rcx`)
					}
					if (shouldReturn) lines.push(`push ${resultRegister}`)

					return
				}
				case 'unary': {
					const ops = {
						'pre++': 'inc',
						'post++': 'inc',
						'pre--': 'dec',
						'post--': 'dec',
						'!': 'not',
						'-': 'neg'
					}

					let varOffset = 0
					let symbol

					if (node.expr.kind == 'reference') {
						symbol = node.expr.symbol
					}
					else {
						assert(node.expr.kind == 'readProp')
						// TODO: proper unwrapping, only support single property access for now
						assert(node.expr.left.kind == 'reference')
						assert(node.expr.prop.kind == 'reference')
						varOffset = node.expr.prop.symbol.offset
						symbol = node.expr.left.symbol
					}

					// pointer deref
					if (node.op == '<~') {
						const type = node.type
						if (node.type.size % 8 != 0) {
							assert(node.type.size == 1)
							// only support bools for now
							// they are stored in 8 bytes xd
							lines.push(`mov rax, ${emitVar(symbol, varOffset)}`)
							lines.push(`push qword [rax]`)
						} else {
							for (let i = type.size - 8; i >= 0; i -= 8) {
								lines.push(`mov rax, ${emitVar(symbol, varOffset)}`)
								lines.push(`push qword [rax + ${i}]`)
							}
						}
						return
					}

					// pointer to address
					if (node.op == '~>') {
						lines.push(`lea rax, ${emitVar(symbol, varOffset)}`)
						lines.push(`push qword rax`)
						return
					}

					const op = ops[node.op]
					assert(op !== undefined, `illegal unary operator ${node.op}`)

					if (shouldReturn && node.op.startsWith('pre')) {
						emitExpr(node.expr)
					}

					if (op == 'not') {
						// doing this because !x should return true when x is any non-zero number
						lines.push(`cmp qword ${emitVar(symbol, varOffset)}, 0`)
						lines.push(`sete al`)
						lines.push(`movzx rax, al`)
						lines.push(`movzx rax, al`)
						lines.push(`push qword rax`)
					} else {
						lines.push(`${op} qword ${emitVar(symbol, varOffset)}`)
						// NOTE: pre and post conditions are emitted before and after
						// if we add more unary operators we should also push their return value onto the stack here
						if (op == 'neg') {
							lines.push(`push qword rax`)
						}
					}

					if (shouldReturn && node.op.startsWith('post')) {
						emitExpr(node.expr)
					}
					return
				}
				case 'numberLiteral': {
					assert(shouldReturn, 'number should not be called at top level')
					if (node.type.tag == tag_float) {
						const [a, b] = f64ToBytes(node.n)
						const fullNumber = '0x' + (b.toString(16).padStart(8, '0')) + a.toString(16).padStart(8, '0')

						// NOTE: we put it into rax first, because pushing a number into the stack directly causes it to be sign extended
						lines.push(`mov rax, ${fullNumber} ; ${node.n}`)
						lines.push(`push rax`)
					} else {
						assert(node.type.tag == tag_int)
						if (node.type.signed) {
							lines.push(`push ${node.n}`)
						} else {
							// NOTE: we put it into rax first, because pushing a number into the stack directly causes it to be sign extended
							lines.push(`mov rax, ${node.n}`)
							lines.push(`push rax`)
						}
					}

					return
				}
				case 'arrayLiteral': {
					assert(
						shouldReturn,
						'array literal should not be called at top level'
					)

					const buffer = emitVar(node)
					assert(buffer)
					assert(node.type && node.type.count)

					lines.push(`; initializing array literal`)
					const isAligned = node.type.of.size % 8 == 0

					if (isAligned) {
						for (let i = 0; i < node.entries.length; i++) {
							emitExpr(node.entries[i])
							for (let j = 0; j < node.type.of.size; j += 8) {
								const offset = i * node.type.of.size + j
								lines.push(`pop rax`)
								lines.push(`mov ${emitVar(node, offset)}, rax`)
							}
						}
					} else {
						assert(node.type.of.size == 1)
						// TODO: make this faster??
						for (let i = 0; i < node.entries.length; i++) {
							emitExpr(node.entries[i])
							lines.push(`pop rax`)
							// NOTE: using al since we're working with 1 byte
							// TODO: work with other sizes
							lines.push(`mov ${emitVar(node, i)}, al`)
						}
					}

					lines.push(`push ${node.type.count}`)
					lines.push(`lea rax, ${buffer}`)
					lines.push(`push rax`)

					return
				}
				case 'booleanLiteral': {
					lines.push(`push qword ${node.value ? 1 : 0} ; ${node.value ? 'true' : 'false'}`)
					return
				}
				case 'charLiteral': {
					lines.push(`push qword '${node.value}'`)
					return
				}
				case 'stringLiteral': {
					assert(
						shouldReturn,
						'string literal should not be called at top level'
					)


					if (node.type.type == 'char') {
						lines.push(`push qword \`${escapeCharSequence(node.value)}\``)
						return
					}

					if (node.type.type == 'cstring') {
						const l = emitVar(node)
						assert(l)
						lines.push(`lea rax, ${l}`)
						lines.push(`push rax`)
					} else {
						assert(node.type.type == 'string')
						const l = emitVar(node)
						assert(l)
						lines.push(`push ${node.len}`)
						lines.push(`lea rax, ${l}`)
						lines.push(`push rax`)
					}

					return
				}
				case 'cast': {
					emitExpr(node.expr)

					switch (node.type.type) {
						case 'int': switch (node.expr.type.type) {
							case 'f64': {
								// pop into xmm0
								lines.push(`pop rax`)
								lines.push(`movq xmm0, rax`)
								// lines.push(`movsd xmm0, qword [rsp]`)
								// lines.push(`add rsp, 8`)

								lines.push(`cvttsd2si rax, xmm0 ; cast f64 -> int`)
								lines.push(`push qword rax`)
							} break
							default: throw node.expr.type
						} break
						case 'f64': switch (node.expr.type.type) {
							case 'int': {
								lines.push(`pop rax`)
								// TODO: see if this is really needed
								lines.push(`pxor xmm0, xmm0`)
								lines.push(`cvtsi2sd xmm0, rax ; cast int -> f64`)

								// push xmm0
								lines.push(`sub rsp, 8`)
								lines.push(`movsd [rsp], xmm0`)
							} break
							default: throw node.expr.type
						} break
						default: throw node.type
					}

					return
				}
				case 'assignVar': {
					if (node.varDec.kind == 'indexedAccess') {
						assert(node.varDec.left.kind == 'reference', 'must be reference')
						assert(node.varDec.left.symbol.kind == 'declareVar' || node.varDec.symbol.kind == 'parameter', 'must reference variable')
						assert(!shouldReturn)

						const varDec = node.varDec.left.symbol
						const index = node.varDec.index
						let indexValue
						if (index.kind == 'numberLiteral') {
							assert(index.type.tag == tag_int)
							indexValue = index.n
						} else {
							emitExpr(index)
							lines.push(`pop r14`)
							indexValue = 'r14'
						}

						// TODO: unhack
						if (varDec.type.type == 'string' || varDec.type.of.size == 1) {
							emitExpr(node.expr)
							lines.push(`; ${varDec.name}[${indexValue}] = expr`)

							lines.push(`mov r15, ${emitVar(varDec)}`)
							lines.push(`add r15, ${indexValue}`)

							lines.push(`pop rax`)
							// NOTE: char is only one byte, so read it from al
							lines.push(`mov [r15], al\n`)

							return
						}

						assert(varDec.type.type == 'array')
						const size = varDec.type.of.size
						assert(size % 8 == 0)

						emitExpr(node.expr)
						lines.push(`; ${varDec.name}[${indexValue}] = expr`)

						lines.push(`mov r15, ${emitVar(varDec)}`)
						if (typeof indexValue == 'number') {
							lines.push(`add r15, ${indexValue * size}`)
						} else {
							lines.push(`mul ${indexValue}, ${size}`)
							lines.push(`add r15, ${indexValue}`)
						}

						let i = 0
						while (i < size) {
							lines.push(`pop rax`)
							lines.push(`mov [r15+${i}], rax\n`)
							i += 8
						}

						return
					}

					assert(node.varDec.kind == 'reference', 'must be reference')
					assert(node.varDec.symbol.kind == 'declareVar' || node.varDec.symbol.kind == 'parameter', 'must reference variable')
					const varDec = node.varDec.symbol

					emitExpr(node.expr)
					lines.push(`; ${varDec.name} = expr`)
					let size = varDec.type.size
					// TODO: properly handle size 1
					if (size == 1) {
						size = 8
					}

					assert(size % 8 == 0)

					let i = 0
					while (i < size) {
						lines.push(`pop rax`)
						lines.push(`mov ${emitVar(varDec, i)}, rax\n`)
						i += 8
					}

					if (shouldReturn) {
						assert(size == 8)
						lines.push(`push rax`)
					}
					return
				}
				default:
					console.log("IT:")
					console.log(node)
					throw `unsupported node kind ${node.kind}`
			}
		}

	}
}

// https://chromium.googlesource.com/chromiumos/docs/+/master/constants/syscalls.md#x86_64-64_bit
/**
console.log([...document.querySelectorAll('table')[1]
.querySelectorAll('tr')]
.filter((v,i)=>i)
.map(tr => {
	const tds = tr.querySelectorAll('td')
	return tds[1].textContent + ':' + tds[3].textContent +','
}).join('\n'))
 */
const syscalls = {
	read: 0x00,
	write: 0x01,
	open: 0x02,
	close: 0x03,
	stat: 0x04,
	fstat: 0x05,
	lstat: 0x06,
	poll: 0x07,
	lseek: 0x08,
	mmap: 0x09,
	mprotect: 0x0a,
	munmap: 0x0b,
	brk: 0x0c,
	rt_sigaction: 0x0d,
	rt_sigprocmask: 0x0e,
	rt_sigreturn: 0x0f,
	ioctl: 0x10,
	pread64: 0x11,
	pwrite64: 0x12,
	readv: 0x13,
	writev: 0x14,
	access: 0x15,
	pipe: 0x16,
	select: 0x17,
	sched_yield: 0x18,
	mremap: 0x19,
	msync: 0x1a,
	mincore: 0x1b,
	madvise: 0x1c,
	shmget: 0x1d,
	shmat: 0x1e,
	shmctl: 0x1f,
	dup: 0x20,
	dup2: 0x21,
	pause: 0x22,
	nanosleep: 0x23,
	getitimer: 0x24,
	alarm: 0x25,
	setitimer: 0x26,
	getpid: 0x27,
	sendfile: 0x28,
	socket: 0x29,
	connect: 0x2a,
	accept: 0x2b,
	sendto: 0x2c,
	recvfrom: 0x2d,
	sendmsg: 0x2e,
	recvmsg: 0x2f,
	shutdown: 0x30,
	bind: 0x31,
	listen: 0x32,
	getsockname: 0x33,
	getpeername: 0x34,
	socketpair: 0x35,
	setsockopt: 0x36,
	getsockopt: 0x37,
	clone: 0x38,
	fork: 0x39,
	vfork: 0x3a,
	execve: 0x3b,
	exit: 0x3c,
	wait4: 0x3d,
	kill: 0x3e,
	uname: 0x3f,
	semget: 0x40,
	semop: 0x41,
	semctl: 0x42,
	shmdt: 0x43,
	msgget: 0x44,
	msgsnd: 0x45,
	msgrcv: 0x46,
	msgctl: 0x47,
	fcntl: 0x48,
	flock: 0x49,
	fsync: 0x4a,
	fdatasync: 0x4b,
	truncate: 0x4c,
	ftruncate: 0x4d,
	getdents: 0x4e,
	getcwd: 0x4f,
	chdir: 0x50,
	fchdir: 0x51,
	rename: 0x52,
	mkdir: 0x53,
	rmdir: 0x54,
	creat: 0x55,
	link: 0x56,
	unlink: 0x57,
	symlink: 0x58,
	readlink: 0x59,
	chmod: 0x5a,
	fchmod: 0x5b,
	chown: 0x5c,
	fchown: 0x5d,
	lchown: 0x5e,
	umask: 0x5f,
	gettimeofday: 0x60,
	getrlimit: 0x61,
	getrusage: 0x62,
	sysinfo: 0x63,
	times: 0x64,
	ptrace: 0x65,
	getuid: 0x66,
	syslog: 0x67,
	getgid: 0x68,
	setuid: 0x69,
	setgid: 0x6a,
	geteuid: 0x6b,
	getegid: 0x6c,
	setpgid: 0x6d,
	getppid: 0x6e,
	getpgrp: 0x6f,
	setsid: 0x70,
	setreuid: 0x71,
	setregid: 0x72,
	getgroups: 0x73,
	setgroups: 0x74,
	setresuid: 0x75,
	getresuid: 0x76,
	setresgid: 0x77,
	getresgid: 0x78,
	getpgid: 0x79,
	setfsuid: 0x7a,
	setfsgid: 0x7b,
	getsid: 0x7c,
	capget: 0x7d,
	capset: 0x7e,
	rt_sigpending: 0x7f,
	rt_sigtimedwait: 0x80,
	rt_sigqueueinfo: 0x81,
	rt_sigsuspend: 0x82,
	sigaltstack: 0x83,
	utime: 0x84,
	mknod: 0x85,
	uselib: 0x86,
	personality: 0x87,
	ustat: 0x88,
	statfs: 0x89,
	fstatfs: 0x8a,
	sysfs: 0x8b,
	getpriority: 0x8c,
	setpriority: 0x8d,
	sched_setparam: 0x8e,
	sched_getparam: 0x8f,
	sched_setscheduler: 0x90,
	sched_getscheduler: 0x91,
	sched_get_priority_max: 0x92,
	sched_get_priority_min: 0x93,
	sched_rr_get_interval: 0x94,
	mlock: 0x95,
	munlock: 0x96,
	mlockall: 0x97,
	munlockall: 0x98,
	vhangup: 0x99,
	modify_ldt: 0x9a,
	pivot_root: 0x9b,
	_sysctl: 0x9c,
	prctl: 0x9d,
	arch_prctl: 0x9e,
	adjtimex: 0x9f,
	setrlimit: 0xa0,
	chroot: 0xa1,
	sync: 0xa2,
	acct: 0xa3,
	settimeofday: 0xa4,
	mount: 0xa5,
	umount2: 0xa6,
	swapon: 0xa7,
	swapoff: 0xa8,
	reboot: 0xa9,
	sethostname: 0xaa,
	setdomainname: 0xab,
	iopl: 0xac,
	ioperm: 0xad,
	create_module: 0xae,
	init_module: 0xaf,
	delete_module: 0xb0,
	get_kernel_syms: 0xb1,
	query_module: 0xb2,
	quotactl: 0xb3,
	nfsservctl: 0xb4,
	getpmsg: 0xb5,
	putpmsg: 0xb6,
	afs_syscall: 0xb7,
	tuxcall: 0xb8,
	security: 0xb9,
	gettid: 0xba,
	readahead: 0xbb,
	setxattr: 0xbc,
	lsetxattr: 0xbd,
	fsetxattr: 0xbe,
	getxattr: 0xbf,
	lgetxattr: 0xc0,
	fgetxattr: 0xc1,
	listxattr: 0xc2,
	llistxattr: 0xc3,
	flistxattr: 0xc4,
	removexattr: 0xc5,
	lremovexattr: 0xc6,
	fremovexattr: 0xc7,
	tkill: 0xc8,
	time: 0xc9,
	futex: 0xca,
	sched_setaffinity: 0xcb,
	sched_getaffinity: 0xcc,
	set_thread_area: 0xcd,
	io_setup: 0xce,
	io_destroy: 0xcf,
	io_getevents: 0xd0,
	io_submit: 0xd1,
	io_cancel: 0xd2,
	get_thread_area: 0xd3,
	lookup_dcookie: 0xd4,
	epoll_create: 0xd5,
	epoll_ctl_old: 0xd6,
	epoll_wait_old: 0xd7,
	remap_file_pages: 0xd8,
	getdents64: 0xd9,
	set_tid_address: 0xda,
	restart_syscall: 0xdb,
	semtimedop: 0xdc,
	fadvise64: 0xdd,
	timer_create: 0xde,
	timer_settime: 0xdf,
	timer_gettime: 0xe0,
	timer_getoverrun: 0xe1,
	timer_delete: 0xe2,
	clock_settime: 0xe3,
	clock_gettime: 0xe4,
	clock_getres: 0xe5,
	clock_nanosleep: 0xe6,
	exit_group: 0xe7,
	epoll_wait: 0xe8,
	epoll_ctl: 0xe9,
	tgkill: 0xea,
	utimes: 0xeb,
	vserver: 0xec,
	mbind: 0xed,
	set_mempolicy: 0xee,
	get_mempolicy: 0xef,
	mq_open: 0xf0,
	mq_unlink: 0xf1,
	mq_timedsend: 0xf2,
	mq_timedreceive: 0xf3,
	mq_notify: 0xf4,
	mq_getsetattr: 0xf5,
	kexec_load: 0xf6,
	waitid: 0xf7,
	add_key: 0xf8,
	request_key: 0xf9,
	keyctl: 0xfa,
	ioprio_set: 0xfb,
	ioprio_get: 0xfc,
	inotify_init: 0xfd,
	inotify_add_watch: 0xfe,
	inotify_rm_watch: 0xff,
	migrate_pages: 0x100,
	openat: 0x101,
	mkdirat: 0x102,
	mknodat: 0x103,
	fchownat: 0x104,
	futimesat: 0x105,
	newfstatat: 0x106,
	unlinkat: 0x107,
	renameat: 0x108,
	linkat: 0x109,
	symlinkat: 0x10a,
	readlinkat: 0x10b,
	fchmodat: 0x10c,
	faccessat: 0x10d,
	pselect6: 0x10e,
	ppoll: 0x10f,
	unshare: 0x110,
	set_robust_list: 0x111,
	get_robust_list: 0x112,
	splice: 0x113,
	tee: 0x114,
	sync_file_range: 0x115,
	vmsplice: 0x116,
	move_pages: 0x117,
	utimensat: 0x118,
	epoll_pwait: 0x119,
	signalfd: 0x11a,
	timerfd_create: 0x11b,
	eventfd: 0x11c,
	fallocate: 0x11d,
	timerfd_settime: 0x11e,
	timerfd_gettime: 0x11f,
	accept4: 0x120,
	signalfd4: 0x121,
	eventfd2: 0x122,
	epoll_create1: 0x123,
	dup3: 0x124,
	pipe2: 0x125,
	inotify_init1: 0x126,
	preadv: 0x127,
	pwritev: 0x128,
	rt_tgsigqueueinfo: 0x129,
	perf_event_open: 0x12a,
	recvmmsg: 0x12b,
	fanotify_init: 0x12c,
	fanotify_mark: 0x12d,
	prlimit64: 0x12e,
	name_to_handle_at: 0x12f,
	open_by_handle_at: 0x130,
	clock_adjtime: 0x131,
	syncfs: 0x132,
	sendmmsg: 0x133,
	setns: 0x134,
	getcpu: 0x135,
	process_vm_readv: 0x136,
	process_vm_writev: 0x137,
	kcmp: 0x138,
	finit_module: 0x139,
	sched_setattr: 0x13a,
	sched_getattr: 0x13b,
	renameat2: 0x13c,
	seccomp: 0x13d,
	getrandom: 0x13e,
	memfd_create: 0x13f,
	kexec_file_load: 0x140,
	bpf: 0x141,
	execveat: 0x142,
	userfaultfd: 0x143,
	membarrier: 0x144,
	mlock2: 0x145,
	copy_file_range: 0x146,
	preadv2: 0x147,
	pwritev2: 0x148,
	pkey_mprotect: 0x149,
	pkey_alloc: 0x14a,
	pkey_free: 0x14b,
	statx: 0x14c
}

module.exports = api
