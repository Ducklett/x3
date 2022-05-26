const { tag_int, tag_float, tag_bool, typeMap, num, readProp, ref, label, goto, binary, cloneType, declareVar, assignVar, indexedAccess, unary, ctor, fn, struct, param, bool, } = require("./ast")
const { typeInfoFor, state, typeEqual } = require("./binder")
const { assert } = require("./util")

function structForEnumEntry(entry) {
	const tag = entry.type.backingType.fields[0]
	const fields = [tag, ...entry.type.sharedFields, ...entry.params]
	const s = struct(entry.name + '$backingStruct', fields)
	assert(s.size <= entry.type.size)
	s.size = entry.type.size

	return s
}

function lower(ast) {
	const { any } = state
	assert(state.any)

	const labelCount = new Map()
	let entrypoint
	let buffers = []
	let breakLabel = null
	let continueLabel = null

	const loweredAst = lowerNodeList(ast)
	return [loweredAst, { entrypoint }]

	function pushBuffer(b) {
		// HACK: hoisting variables created by lowerer
		if (b.kind == 'function') {
			const lowered = lowerNode(b)
			for (let l of lowered) buffers.push(l)
		}
		else if (b.kind == 'declareVar') {
			/*
			can't accept variables with expressions here,
			imagine the following scenario:

			var a = 10
			print(!a)
			
			in order we get:
			- buffer a
			- initializer for buffer a
			- buffer v (holds !a)
			  - expression for buffer v (in buffers array)
			- print call


			the buffers are emitted first,
			so we end up running !a before the a initializer
			*/
			assert(!b.expr)
			const lowered = lowerNode(b)
			assert(lowered.length == 1)
			for (let l of lowered) buffers.push(l)
		} else {
			buffers.push({ kind: 'buffer', data: b })
		}
	}

	function mangleLabel(name) {
		name = name.replace(/\s/g, '_')

		const count = labelCount.get(name) ?? 0
		labelCount.set(name, count + 1)
		const newName = count == 0
			? name
			// : name + btoa(count).replace(/=/g, '')
			: name + Buffer.from([count]).toString('base64').replace(/=/g, '')
		return newName
	}

	function mangleName(node) {
		function mangleSegment(str) {
			return str.replace(/\s/g, '_')
		}
		assert(node.name)

		let left = mangleSegment(node.name)
		if (node.scope) {
			let scope = node.kind == 'declareVar' ? node.scope : node.scope.parent
			while (scope) {
				if (scope.kind == 'file') break
				assert(scope.kind != 'global')

				if (scope.name) left = mangleSegment(scope.name) + '__' + left
				scope = scope.parent
			}
		}

		return left
	}

	/*
	lowers a list of nodes and returns the new list
	if excessTarget is provided this buffer will be populated with all but the last returned item
	why we need excessTarget:

	a(b())
	
	depending on the type signature of a and b we might have
	all kind of casting magin going on begin the scenes
	these casts may need to allocate memory on the stack
	normally this would look like this
	- push arg 1
	- declare tmpvar (!!!)
	- assign tmpvar (!!!)
	- push arg 2
	- call

	with excessTarget we can hoist this mess so it becomes this
	excess:
	- declare tmpvar
	- assign tmpvar

	returned:
	- push arg1
	- push arg2

	now the caller of lowerNodeList can decide an appropriate spot to put these instructions
	*/
	function lowerNodeList(nodes, excessTarget) {
		if (!nodes) return null
		const newList = []
		for (let n of nodes) {
			const result = lowerNode(n)

			if (excessTarget) {
				assert(result?.length >= 1)
				for (let i = 0; i < result.length - 1; i++) {
					const r = result[i]
					assert(!Array.isArray(r))
					excessTarget.push(r)
				}

				newList.push(result[result.length - 1])
			} else if (result) {
				assert(Array.isArray(result))
				for (let r of result) {
					assert(!Array.isArray(r))
					newList.push(r)
				}
			}
		}
		return newList
	}

	function lowerNode(node, { makeBuffer = true, asTag = false } = {}) {
		switch (node.kind) {
			case 'import':
			case 'type alias':
			case 'enum':
			case 'use': return []

			case 'unary':
			case 'numberLiteral':
			case 'booleanLiteral':
			case 'charLiteral': return [node]

			case 'named argument': return lowerNode(node.expr)

			case 'sizeof': {
				assert(node.arg)
				const size = node.arg.size || node.arg.type.size
				assert(size !== undefined)

				const theSize = num(node.arg.size, node.type)
				return lowerNode(theSize)
			}
			case 'spread': {
				const arr = {
					kind: 'arrayLiteral',
					entries: node.args,
					type: cloneType(node.type),
				}

				arr.type.count = node.args.length

				return lowerNode(arr)
			}
			case 'cast': {
				const expr = lowerNode(node.expr)
				assert(expr.length == 1)
				node.expr = expr[0]

				if (node.expr.kind == 'numberLiteral' && node.type.tag == tag_int) {
					node.expr.type = node.type
					return [node.expr]
				}

				return [node]
			}
			case 'reinterpret': return lowerNode(node.expr)
			case 'implicit cast': {

				if (typeEqual(node.type, node.expr.type)) {
					console.log(node)
					throw `bug: cast to own type`
				}

				const toLabel = node.type.type
				const fromLabel = node.expr.type.type

				if (fromLabel == 'cstring' && toLabel == 'pointer') return lowerNode(node.expr)
				if (fromLabel == 'pointer' && toLabel == 'pointer') return lowerNode(node.expr)
				if (fromLabel == 's64' && toLabel == 'int') return lowerNode(node.expr)
				if (fromLabel == 'int' && toLabel == 's64') return lowerNode(node.expr)
				if (fromLabel == 'int' && toLabel == 'u64') {
					node.kind = 'cast'
					return lowerNode(node)
				}
				if (fromLabel == 'int' && toLabel == 'uint') {
					node.kind = 'cast'
					return lowerNode(node)
				}
				if (fromLabel == 'uint' && toLabel == 'int') {
					node.kind = 'cast'
					return lowerNode(node)
				}

				if (fromLabel == 'void' && node.type.tag == tag_int && node.type.size < 8) {
					// might need to do some cleanup before deref
					node.kind = 'cast'
					return lowerNode(node)
				}

				if (node.type.kind == 'enum' && node.expr.kind == 'reference' && node.expr.symbol.kind == 'enum alias') {
					assert(node.expr.type.kind == 'struct')
					assert(node.expr.type.size == node.type.size)
					return lowerNode(node.expr)
				}



				if (node.expr.type.type == 'void') {
					// we allow dereferencing of void pointers when the expected type can be inferrect from context
					// to make this work we replace the void type with that of the implicit cast before emitting to asm
					node.expr.type = node.type
					return lowerNode(node.expr)
				}

				if (node.type.type == 'array' && node.expr.type.type == 'array') {
					assert(node.type.of.type == 'any')
					const count = node.expr.type.count
					assert(count !== undefined)
					node.expr.type = cloneType(node.type)
					node.expr.type.count = count

					return lowerNode(node.expr)
				}

				if (node.type.type == 'any') {
					const toReturn = []

					// let loweredExpr = lowerNode(node.expr)
					// assert(loweredExpr.length == 1)
					// loweredExpr = loweredExpr[0]

					const isEnumEntry =
						(node.expr.kind == 'reference' && node.expr.symbol.kind == 'enum entry') ||
						(node.expr.kind == 'readProp' && node.expr.prop.symbol.kind == 'enum entry')

					// the expression MUST refer to some memory address at the end of the day, because we have to point to it
					// if we encounter any immediates we will first have to store them in memory and then reference their address
					if (node.expr.type.type == 'void') {
						// just reinterpret
						node.expr.type = node.type
						return lowerNode(node.expr)
					}
					else if (isEnumEntry || (node.expr.kind != 'reference' && node.expr.kind != 'readProp')) {
						const v = declareVar(mangleLabel('v'), node.expr)

						const stuff = lowerNode(v)
						assert(stuff.length >= 2)

						toReturn.push(...stuff)

						node.expr = ref(v)
					}

					let loweredExpr = lowerNode(node.expr)

					assert(loweredExpr.length == 1)
					loweredExpr = loweredExpr[0]

					if (loweredExpr.kind == 'stringLiteral') {
						console.log(node.expr)
						throw 'how'
					}

					if (loweredExpr.kind == 'ctorcall' && loweredExpr.type.type == 'any') {
						return [loweredExpr]
					}

					if (!(loweredExpr.kind == 'reference' || loweredExpr.kind == 'readProp')) {
						console.log(node.expr)
						console.log(loweredExpr)
					}
					assert(loweredExpr.kind == 'reference' || loweredExpr.kind == 'readProp')
					const tPtr = cloneType(typeMap.pointer)
					tPtr.to = node.expr.type

					const expr = unary('~>', loweredExpr, tPtr)

					// NOTE: we are using the raw expr type. not the lowered one
					// this is because enums get lowered to their backing type but we still want to print them as enums
					const exprTypeInfo = ref(typeInfoFor(node.expr.type))
					// HACK: make type a pointer because globals are always emitted as pointers
					const ptr = cloneType(typeMap.pointer)
					ptr.to = exprTypeInfo.type
					exprTypeInfo.type = ptr

					const data = ctor(any, expr, exprTypeInfo)

					toReturn.push(data)
					return toReturn
				}

				if (node.type.type == 'pointer' && (node.expr.type.type == 'string' || node.expr.type.type == 'array')) {
					const bufferProp = node.expr.type.type == 'string'
						? typeMap.string.scope.symbols.get('buffer')
						: typeMap.array.scope.symbols.get('buffer')
					assert(bufferProp)

					assert(node.expr.kind == 'reference')
					const bufferAccess = readProp(node.expr, ref(bufferProp))
					return lowerNode(bufferAccess)
				}

				if (node.type.size != node.expr.type.size) {
					const padding = node.type.size - node.expr.type.size
					assert(padding > 0)
					const pad = {
						kind: 'pad',
						padding,
						expr: node.expr,
					}
					return lowerNode(pad)
				}

				throw `unhandled implicit cast from ${node.expr.type.type} to ${node.type.type}`
				// return lowerNode(node.expr)
			}

			case 'pad': {
				const expr = lowerNode(node.expr)
				assert(expr.length == 1)
				node.expr = expr[0]
				return [node]
			}

			case 'postUnary': {
				const expr = lowerNode(node.expr)
				assert(expr.length == 1)
				node.expr = expr[0]
				if (node.op == '++') node.op = 'post++'
				else if (node.op == '--') node.op = 'post--'

				node.kind = 'unary'

				return [node]
			}
			case 'preUnary': {
				const expr = lowerNode(node.expr)
				assert(expr.length == 1)
				node.expr = expr[0]

				if (node.op == '-' && node.expr.kind == 'numberLiteral') {
					node.expr.n = -node.expr.n
					return [node.expr]
				}

				if (node.op == '++') node.op = 'pre++'
				else if (node.op == '--') node.op = 'pre--'

				node.kind = 'unary'

				return [node]
			}

			case 'label': {
				node.name = mangleLabel(node.name)
				return [node]
			}

			case 'goto': {
				if (node.condition) {
					const cond = lowerNode(node.condition)
					assert(cond.length == 1)
					node.condition = cond[0]
				}
				return [node]
			}

			case 'nullLiteral': { return lowerNode(num(0, typeMap.u64)) }
			case 'stringLiteral': {

				if (makeBuffer) {
					if (node.type.type != 'char') {
						if (buffers.includes(node)) {
							// this can legitimately happen if the string is referenced twice
							// throw "bruh"
						} else {
							pushBuffer(node)
						}
					}
				}

				return [node]
			}
			case 'bufferLiteral': {
				const toReturn = []
				node.entries = lowerNodeList(node.entries, toReturn)
				toReturn.push(node)
				return toReturn
			}
			case 'arrayLiteral': {
				const toReturn = []
				node.entries = lowerNodeList(node.entries, toReturn)

				if (makeBuffer) {
					if (buffers.includes(node)) {
						// throw "bruh"
					} else {
						pushBuffer(node)
					}
				}

				// how it works:
				// - array is split into 'array' and 'buffer'
				// - the buffer is allocated at the start of the function
				// - the array is allocated when needed; it points to the buffer
				//
				// if we don't do this we get problems with functions calls:
				// push number
				// push buffer
				// push array (containing data)
				// push number
				// call foo()
				//
				// foo takes the number and array off the stack, then it starts eating the instead of the last number, because that's the next up on the stack

				// actually this is gonna be a nightmare with the current lowererer; so i'm just gonna do a hack in the x86 part for now

				toReturn.push(node)

				return toReturn
			}
			case 'while': {
				// goto condition
				// label begin:
				// body
				// condition:
				// if condition goto begin

				let conditionLabel = label('condition')
				let beginLabel = label('begin')
				let jumpToCondition = goto(conditionLabel)
				let body = node.block
				let jumpToBegin = goto(beginLabel, node.condition)
				let endLabel = label('end')

				let prevBreak = breakLabel
				let prevContinue = continueLabel
				breakLabel = endLabel
				continueLabel = conditionLabel
				const result = lowerNodeList([jumpToCondition, beginLabel, body, conditionLabel, jumpToBegin, endLabel])
				breakLabel = prevBreak
				continueLabel = prevContinue

				return result
			}
			case 'for': {
				// precondition
				// goto condition
				// label begin:
				// body
				// postcondition
				// condition:
				// if condition goto begin

				let conditionLabel = label('condition')
				let beginLabel = label('begin')
				let jumpToCondition = goto(conditionLabel)
				let body = node.block
				let precondition = node.preCondition ?? nop()
				let jumpToBegin = goto(beginLabel, node.condition)
				let postCondition = node.postCondition ?? nop()
				let endLabel = label('end')

				let prevBreak = breakLabel
				let prevContinue = continueLabel
				breakLabel = endLabel
				continueLabel = conditionLabel
				const result = lowerNodeList([precondition, jumpToCondition, beginLabel, body, postCondition, conditionLabel, jumpToBegin])
				breakLabel = prevBreak
				continueLabel = prevContinue
				return result
			}
			case 'each': {
				const isInt = node.list.type.type == 'int'

				let indexInitializer = num(0, cloneType(typeMap.int))
				if (isInt) {
					node.item.expr = indexInitializer
				} else {
					if (node.index) node.index.expr = indexInitializer
				}
				let i = isInt ? node.item : node.index ?? declareVar('i', indexInitializer)
				let begin = label('begin')
				let endLabel = label('end')
				let cont = label('continue')
				const lengthProp = typeMap.string.scope.symbols.get('length')
				const readLength = readProp(ref(node.list), ref(lengthProp))
				let condition = goto(endLabel, binary('>=', ref(i), isInt
					? ref(node.list)
					: readLength
				))
				let item, setItem
				if (!isInt) {
					item = node.item
					setItem = assignVar(ref(item), indexedAccess(ref(node.list), ref(i)))
				}
				let body = node.block
				let inc = unary('post++', ref(i))
				let loop = goto(begin)

				let prevBreak = breakLabel
				let prevContinue = continueLabel
				breakLabel = endLabel
				continueLabel = cont
				const result = isInt
					? lowerNodeList([i, begin, condition, body, cont, inc, loop, endLabel])
					: lowerNodeList([i, item, begin, condition, setItem, body, cont, inc, loop, endLabel])
				breakLabel = prevBreak
				continueLabel = prevContinue
				return result
			}
			case 'break': {
				assert(breakLabel !== null)
				return [goto(breakLabel)]
			}
			case 'continue': {
				assert(continueLabel !== null)
				return [goto(continueLabel)]
			}
			case 'return': {
				if (node.expr) {
					const expr = lowerNode(node.expr)
					assert(expr.length && expr.length == 1)
					node.expr = expr[0]
				}
				return [node]
			}

			case 'parenthesized expression': {
				const expr = lowerNode(node.expr)
				return expr
			}

			case 'module': return lowerNodeList(node.declarations.statements)

			case 'do': return lowerNodeList(node.instructions.statements)
			case 'match': {
				// op = <expr>
				// goto block1 if <match>
				// goto block2 if <match>
				// goto end
				// <block1>
				// goto end
				// <block2>
				// goto end
				// end:

				// we don't want to evaluate the expression multiple times, store it in a variable
				const operand = declareVar('matchOperand', node.operand)
				const endLabel = label('end')
				const gotoEnd = goto(endLabel)
				const conditions = []
				const blocks = []

				let matchExpr

				for (let arm of node.arms) {
					const blockLabel = label('block')

					switch (arm.pattern.kind) {
						case 'pattern else': {
							// match unconditionally
							matchExpr = null
						} break
						case 'pattern expression': {
							matchExpr = arm.pattern.expr
						} break
						case 'pattern value': {
							const matchexpression = binary('==', arm.pattern.value, ref(operand), typeMap.bool)

							matchExpr = matchexpression
						} break
						case 'pattern equal': {
							assert(arm.pattern.value.kind == 'enum entry')

							if (arm.pattern.alias) {
								arm.pattern.alias.of = operand
							}

							let tagRef

							if (!operand.type.backingType.fields) {
								// basic enum
								tagRef = ref(operand)
							} else {
								// tagged union
								const tag = operand.type.backingType.fields[0]
								tagRef = readProp(ref(operand), ref(tag))
							}

							const matchexpression = binary('==', tagRef, arm.pattern.value, typeMap.bool)

							matchExpr = matchexpression
						} break
						default: throw `unhandled pattern ${arm.pattern.kind}`
					}

					const blockGoto = goto(blockLabel, matchExpr)
					const block = arm.body

					conditions.push(blockGoto)
					blocks.push(blockLabel)
					blocks.push(block)
					blocks.push(gotoEnd)

				}

				conditions.push(gotoEnd)

				const it = [operand, ...conditions, ...blocks, endLabel]
				return lowerNodeList(it)
			}
			case 'if': {

				// goto then if (cond)
				// else:
				// <else block
				// goto end
				// then:
				// <then block>
				// end:

				// const cond = lowerNode(node.cond)
				// assert(cond.length && cond.length == 1)
				const thenLabel = label('then')
				const elseLabel = label('else')
				const endLabel = label('end')
				const gotoThen = goto(thenLabel, node.cond)
				const gotoEnd = goto(endLabel)
				// const thenBlock = lowerNodeList(node.then.statements)
				let thenBlock = node.then.kind == 'block' ? node.then.statements : [node.then]
				const elseBlock = !node.els
					? [] : node.els.kind == 'block'
						? node.els.statements
						: [node.els]

				return lowerNodeList([gotoThen, elseLabel, ...elseBlock, gotoEnd, thenLabel, ...thenBlock, endLabel])

				// node.then = lowerNodeList(node.then.statements)
				// if (node.els) {
				//     if (node.els.kind == 'if') {
				//         node.els = lowerNode(node.els)
				//     } else {
				//         assert(node.els.kind == 'block')
				//         node.els = lowerNodeList(node.els.statements)
				//     }
				// }
				// return [node]
			}

			case 'declareVar': {
				const toReturn = []

				node.name = mangleName(node)
				let expr

				if (!node.expr && node.type.type == 'array' && node.type.count) {
					const shouldZeroInitialize = !node.notes.has('noinit')
					node.expr = {
						kind: 'arrayLiteral',
						// when entries are null the x64 backend will zero initialize the memory block
						entries: null,
						shouldZeroInitialize,
						type: node.type
					}
				}

				if (node.notes.has('const')) {
					assert(node.expr)
					// NOTE: we DONT want to split string into a buffer because it's a true constant
					// it should end up in the data section instead.
					expr = lowerNode(node.expr, { makeBuffer: false })
					assert(expr.length == 1)
					node.expr = expr[0]

					// inlined
					// TODO: still add some comment to nasm so we know what the number represents
					if (node.expr.kind == 'numberLiteral') {
						return []
					}

					const isTrueConstant = new Set(['stringLiteral', 'boolLiteral', 'arrayLiteral', 'ctorcall'])
					if (isTrueConstant.has(node.expr.kind)) {
						return [node]
					}

					// just treat it as a normal variable for now
					// only difference is that we prevent you from reassigning the value
				}

				if (node.expr) {
					const varDec = node
					// don't lower if we already did so above
					if (!expr) {
						expr = node.expr ? lowerNode(node.expr) : null
					}

					if (expr) {
						assert(expr.length > 0)
						for (let i = 0; i < expr.length - 1; i++) {
							toReturn.push(expr[i])
						}
						expr = expr[expr.length - 1]
					}
					const ref = { kind: 'reference', symbol: varDec, type: varDec.type }
					const varAssignment = { kind: 'assignVar', varDec: ref, expr }
					varDec.expr = undefined

					toReturn.push(varDec)
					toReturn.push(varAssignment)
					return toReturn
				} else {
					toReturn.push(node)
					return toReturn
				}
			}
			case 'function': {
				// NOTE: we can't create a new copy because this would break the symbol
				node.name = mangleName(node)

				// labdas need their own buffer, so store the previous buffer on the stack and restore it when we're done
				let prevBuffer = buffers
				buffers = []

				const isEntrypoint = node.notes.has('entrypoint')
				if (isEntrypoint) {
					assert(!entrypoint)
					entrypoint = node;
				}
				const unloweredInstructions = Array.isArray(node.instructions)
					? node.instructions
					: node.instructions?.statements
				let instructions = lowerNodeList(unloweredInstructions)

				const outInstructions = []
				const outDeclarations = [node]

				if (instructions) {
					instructions = [...buffers, ...instructions]

					for (let instr of instructions) {
						if (instr.kind == 'function') {
							outDeclarations.push(instr)
						} else {
							outInstructions.push(instr)
						}
					}

					node.instructions = outInstructions //[...buffers, ...outInstructions]
				}
				buffers = prevBuffer
				return outDeclarations
			}
			case 'lambda': {
				const name = mangleLabel('lambda')
				const asFn = fn(name, node.params, node.returnType, node.instructions.statements, node.type)
				pushBuffer(asFn)
				const asRef = ref(asFn)
				return lowerNode(asRef)
			}

			case 'union':
			case 'struct': {
				node.name = mangleName(node)
				return []
			}

			case 'block': return lowerNodeList(node.statements)

			case 'binary': {
				if (node.b.alias) {
					assert(node.a.kind == 'reference')
					node.b.alias.of = node.a.symbol
				}

				if (node.type?.kind == 'struct') {
					assert(node.type.notes.has('arithmetic'),
						'operators are only implemented for #arithmetic structs')

					// apply the operator piecewise, the only valid use case ;)
					const fields = []
					for (let i = 0; i < node.type.fields.length; i++) {
						const left = readProp(node.a, ref(node.type.fields[i]))
						const right = readProp(node.b, ref(node.type.fields[i]))
						const bin = binary(node.op, left, right)
						fields.push(bin)
					}
					const it = ctor(node.type, ...fields)
					return lowerNode(it)
				}

				const left = lowerNode(node.a)
				const right = lowerNode(node.b, { asTag: true })
				assert(left.length == 1)
				assert(right.length == 1)
				node.a = left[0]
				node.b = right[0]

				// HACK: bad constant folding
				if (node.a.kind == 'numberLiteral' && node.b.kind == 'numberLiteral') {

					assert(node.type.tag == tag_int || node.type.tag == tag_float || node.type.tag == tag_bool)

					// NOTE: tight coupling between x3 and js operators
					const newValue = eval(`node.a.n ${node.op} node.b.n`)

					let it
					if (node.type.tag == tag_bool) {
						it = bool(newValue, typeMap.bool)
					} else {
						it = { ...node.a, n: Number(newValue), type: node.type }
					}
					return [it]

				}

				return [node]
			}

			case 'assignVar': {
				if (node.op && node.op !== '=') {
					const opLen = node.op.length - 1
					const binaryOp = node.op.slice(0, opLen)
					node.op = '='
					node.expr = binary(binaryOp, node.varDec, node.expr)
				}

				const vardec = lowerNode(node.varDec)
				assert(vardec.length == 1)
				node.varDec = vardec[0]

				if (node.varDec.kind == 'readProp') {
					const left = node.varDec.left
					const right = node.varDec.prop
					const expr = node.expr
					const it = { kind: 'assignProp', left, right, expr, span: node.span }
					return lowerNode(it)
				}

				const expr = lowerNode(node.expr)

				assert(expr.length == 1)
				node.expr = expr[0]
				return [node]
			}
			case 'reference': {
				switch (node.symbol.kind) {
					case 'enum entry':
						if (node.alias) {
							return lowerNode(node.symbol, { asTag: true })
						} else {
							return lowerNode(node.symbol)
						}
					case 'enum alias': {
						// TODO: check if this case should exist or is the result of a compiler bug
						assert(node.symbol.of)
						return lowerNode(ref(node.symbol.of))
					}
					case 'declareVar': {
						if (node.symbol.notes.has('const') && node.symbol.expr) {
							// HACK: typeinfo is stored as pointer and should not be inlined
							// (typeinfo will be of size 8 since pointer has size 8)
							if (node.type.type != 'pointer') {
								// inline little constants
								if (node.type.size <= 8) {
									return lowerNode(node.symbol.expr)
								}
							}
						}

						return [node]
					}
					case 'parameter': return [node]
					case 'function': return [node]
					// this will be handled in readProp
					case 'module': return [node]

					default:
						assert(false, `unhandled kind ${node.symbol.kind}`)
						break;
				}
			}

			case 'pipe': {
				assert(node.call.kind == 'reference' && node.call.symbol.kind == 'function')
				const it = {
					kind: 'call',
					def: node.call,
					args: [node.left],
					type: node.type,
					span: node.span
				}
				return lowerNode(it)
			}

			case 'enumctorcall': {
				const type = structForEnumEntry(node.entry)

				const backingType = node.type.backingType
				assert(backingType)
				const tagType = backingType.fields[0].type
				assert(tagType)

				const args = [num(node.entry.value, tagType), ...(node.args ?? [])]

				const c = ctor(type, ...args)
				return lowerNode(c)
			}
			case 'ctorcall':
			case 'syscall':
			case 'call': {
				const toReturn = []

				const args = (node.args ?? []).map(a => {
					const result = lowerNode(a)
					assert(result.length > 0)

					// arg may return several instructions
					// execute all but the last before we start doing the call
					for (let i = 0; i < result.length - 1; i++) {
						toReturn.push(result[i])
					}

					return result[result.length - 1]
				})

				toReturn.push({ ...node, args })
				return toReturn
			}
			case 'indexedAccess': {
				const expr = lowerNode(node.index)
				assert(expr.length == 1)
				node.index = expr[0]
				return [node]
			}
			case 'enum entry': {
				if (node.type.size <= 8 || asTag) {
					// NOTE: only returns the tag, enum instances with values are instead stored in 'enumctor'
					const tagType = node.type.backingType.fields
						? node.type.backingType.fields[0].type
						: node.type.backingType
					const numericRepresentation = num(node.value, tagType)
					return lowerNode(numericRepresentation)
				} else {
					console.log(node)
					throw 'tried to lower enum entry but asTag was set to false'
				}
			}
			case 'assignProp': {
				const left = lowerNode(node.left)
				const right = lowerNode(node.right)
				const expr = lowerNode(node.expr)
				assert(left.length == 1)
				assert(right.length == 1)
				assert(expr.length == 1)
				node.left = left[0]
				node.right = right[0]
				node.expr = expr[0]
				return [node]
			}
			case 'readProp': {

				while (node.left.kind == 'readProp') {
					let left = lowerNode(node.left)
					assert(left.length == 1)
					left = left[0]
					if (left == node.left) {
						// done lowering this side
						break
					}
					node.left = left
				}

				if (node.left.kind == 'reference') {
					const symbol = node.left.symbol
					switch (symbol.kind) {
						case 'parameter': return [node]
						case 'module': return lowerNode(node.prop)
						case 'declareVar': return [node]
						case 'enum alias': {
							assert(symbol.of)
							const r = ref(symbol.of)
							const prop = readProp(r, node.prop)
							return lowerNode(prop)
						}
						case 'enum': {
							if (node.prop.kind == 'enumctorcall') return lowerNode(node.prop)

							assert(node.prop.kind == 'reference')
							assert(node.prop.symbol.kind == 'enum entry')

							const hasFields = symbol.backingType.fields?.length > 0
							const tagType = hasFields
								? symbol.backingType.fields[0].type
								: symbol.backingType

							const tag = num(node.prop.symbol.value, tagType)
							if (hasFields) {
								// the source code was 'enum.entry' but the the enum has fields
								// we must turn it into a constructor call so it has the right size
								const entry = node.prop.symbol

								assert(entry.type.sharedFields.length === 0, `cannot reference entry ${entry.name} because it has shared arguments, construct it with ${entry.name}() instead.`)
								assert(entry.params.length == 0, `cannot reference entry ${entry.name} because it has arguments, construct it with ${entry.name}() instead.`)

								const s = structForEnumEntry(entry)//struct(entry.name + '$backingStruct', [param('tag', tagType)])
								// s.size = entry.type.size

								const c = ctor(s, tag)

								return lowerNode(c)
							} else {
								// it's just a number
								return lowerNode(tag)
							}
						}
						default:
							console.log(node)
							assert(false, `unhandled kind ${symbol.kind}`)
					}
				}
				return [node]
			}
			default:
				console.log('??')
				console.log(node)
				throw `lowering not implemented for ${node.kind}`
		}
	}
}

module.exports = { lower }
