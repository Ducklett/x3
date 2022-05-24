const { tag_int, tag_float, tag_bool, roundToIncrement } = require("./ast")
const { assert, write, escapeString } = require("./util")

const api = {
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

								let size = cur.data.type.count * cur.data.type.of.size
								if (size % 8 != 0) {
									size = Math.ceil(size / 8) * 8
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

								lines.push(`; ${escapeString(node.value)}`)
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

								// NOTE: we will only initialize the members once we emit the array literal
								let size = node.type.count * node.type.of.size
								if (size * 8 != 0) {
									size = Math.ceil(size / 8) * 8
								}
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
					// let size = args.reduce((acc, cur) => acc + cur.type.size, 0)
					// assert(!isNaN(size))

					// while (size < targetSize) {
					// 	lines.push('push qword 0 ; end padding')
					// 	size += 8
					// }

					// emitArgs(args)

					// args correspond directly to fields; so we can steal the offset
					assert(node.type.fields.length == node.args.length)

					assert(targetSize % 8 == 0)
					const size = targetSize //roundToIncrement(targetSize, 8)
					lines.push(`sub rsp, ${size}`)

					let i = 0
					for (let arg of args) {
						// assert(arg.type.size % 8 == 0, `don't small values for now`)
						emitExpr(arg)
						const argSize = arg.type.size
						let exprSizeOnStack = roundToIncrement(arg.type.size, 8)
						const offset = node.type.fields[i].offset
						assert(offset !== undefined)

						lines.push(`add rsp, ${exprSizeOnStack}`)


						if (argSize >= 8) {
							for (let j = 0; j < exprSizeOnStack; j += 8) {
								lines.push(`mov rax, [rsp-${exprSizeOnStack - j}]`)
								lines.push(`mov [rsp+${offset + j}], rax`)
							}
						} else {
							// small values are always emitted as qwords
							lines.push(`mov rax, [rsp-${exprSizeOnStack}]`)

							if (argSize > 4) {
								lines.push(`mov [rsp+${offset}], rax`)
							} else if (argSize > 2) {
								lines.push(`mov [rsp+${offset}], eax`)
							} else if (argSize == 2) {
								lines.push(`mov [rsp+${offset}], ax`)
							} else {
								lines.push(`mov [rsp+${offset}], al`)
							}
						}
						i++
					}

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

					function pushVar(varLabel, size) {
						lines.push(`; push ${varLabel.name}`)

						// NOTE: size is always being rounded up to a factor of 8 bytes
						size = roundToIncrement(size, 8)

						lines.push(`sub rsp, ${size}`)

						let off = 0
						while (off < size) {
							lines.push(`mov rax, ${emitVar(varLabel, off)}`)
							lines.push(`mov qword [rsp+${off}], rax`)
							off += 8
						}

						// let offset = 0
						// assert(size > 0)
						// while (offset < size) {
						// 	if (size - offset >= 8) {
						// 		lines.push(`mov rax, ${emitVar(varLabel, offset)}`)
						// 		lines.push(`mov [rsp+${offset}], rax`)
						// 		offset += 8
						// 	} else if (size - offset >= 4) {
						// 		lines.push(`mov eax, ${emitVar(varLabel, offset)}`)
						// 		lines.push(`mov [rsp+${offset}], eax`)
						// 		offset += 4
						// 	} else if (size - offset >= 2) {
						// 		lines.push(`mov ax, ${emitVar(varLabel, offset)}`)
						// 		lines.push(`mov [rsp+${offset}], ax`)
						// 		offset += 2
						// 	} else {
						// 		lines.push(`mov al, ${emitVar(varLabel, offset)}`)
						// 		lines.push(`mov [rsp+${offset}], al`)
						// 		offset += 1
						// 	}
						// }
						// assert(offset == size)
					}

					pushVar(node.symbol, size)

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

					assert(prop.kind == 'reference')
					assert(prop.symbol.offset !== undefined)

					offset += prop.symbol.offset

					const size = prop.symbol.type.size

					if (left.type.type == 'pointer') {
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

					emitExpr(node.a)
					emitExpr(node.b)


					if (node.a.type.tag == tag_float) {
						assert(node.b.type.tag == tag_float)
						const ops = {
							'+': 'addsd',
							'-': 'subsd',
							'*': 'mulsd',
							'/': 'divsd',

							'>': 'seta',
							'>=': 'setnb',
							'<': 'setb',
							'<=': 'setna',
							'==': 'sete',
							'!=': 'setne',
						}
						const op = ops[node.op]
						assert(op, `illegal binary operator ${node.op}`)

						// pop b
						lines.push(`pop rax`)
						lines.push(`movq xmm1, rax`)

						// pop a
						lines.push(`pop rax`)
						lines.push(`movq xmm0, rax`)

						if (op.startsWith('set')) {
							if (op == 'sete') {
								assert(false, `steal complicated shit from gcc`)
							} else if (op == 'setne') {
								assert(false, `steal complicated shit from gcc`)
							} else {
								lines.push(`comisd xmm0, xmm1`)
								lines.push(`${op} al`)
								lines.push(`movzx rax, al`)
							}
						} else {
							lines.push(`${op} xmm0, xmm1`)
							lines.push(`movq rax, xmm0`)
						}

						if (shouldReturn) lines.push(`push rax`)
					} else {
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
					}


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
						if (node.expr.type.tag == tag_bool) {
							// doing this because !x should return true when x is any non-zero number
							lines.push(`cmp qword ${emitVar(symbol, varOffset)}, 0`)
							lines.push(`sete al`)
							lines.push(`movzx rax, al`)
							lines.push(`push qword rax`)
						} else {
							assert(node.expr.type.tag == tag_int)
							assert(!node.expr.type.signed)
							lines.push(`mov rax, ${emitVar(symbol, varOffset)}`)
							lines.push(`not rax`)
							lines.push(`push qword rax`)
						}
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
						if (node.type.tag != tag_int) {
							console.log(node)
						}
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
					assert(node.type)

					lines.push(`; initializing array literal`)

					const count = node.entries?.length ?? node.type?.count ?? null
					assert(count !== null)

					const size = count * node.type.of.size

					function zeroInitialize(varKey, size) {
						let offset = 0
						while (size > 0) {
							if (size >= 8) {
								lines.push(`mov qword ${emitVar(varKey, offset)}, 0`)
								size -= 8
								offset += 8
							} else if (size >= 4) {
								lines.push(`mov dword ${emitVar(varKey, offset)}, 0`)
								size -= 4
								offset += 4
							} else if (size >= 2) {
								lines.push(`mov word ${emitVar(varKey, offset)}, 0`)
								size -= 2
								offset += 2
							} else {
								lines.push(`mov byte ${emitVar(varKey, offset)}, 0`)
								size -= 1
								offset += 1
							}
						}
					}


					if (node.entries) {
						const isAligned = node.type.of.size % 8 == 0
						if (isAligned) {
							for (let i = 0; i < count; i++) {
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
							for (let i = 0; i < count; i++) {
								emitExpr(node.entries[i])
								lines.push(`pop rax`)
								// NOTE: using al since we're working with 1 byte
								// TODO: work with other sizes
								lines.push(`mov ${emitVar(node, i)}, al`)
							}
						}
					} else {
						if (node.shouldZeroInitialize) {
							zeroInitialize(node, size)
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
						lines.push(`push qword \`${escapeString(node.value)}\``)
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
						case 's64':
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
							lines.push(`imul ${indexValue}, ${size}`)
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

module.exports = api
