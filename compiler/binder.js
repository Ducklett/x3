const { typeMap, cloneType, typeInfoLabel, declareVar, num, binary, ref, readProp, unary, label, goto, assignVar, indexedAccess, nop, call, ctor, struct, param, fn, str, MARK, union, bool, roundToIncrement, tag_void, tag_pointer, tag_int, tag_float, tag_string, tag_array, tag_char, tag_bool, tag_function, tag_struct, tag_enum } = require('./ast')
const { assert, spanFromRange } = require('./util')
const { fileMap } = require('./parser')

const state = {
	fileScopes: null,
	scopeStack: null,
	any: null,
	typeInfo: null,
	ast: null,
	globalScope: null,
	fieldData: null,
	pointerData: null,
	structData: null,
	enumData: null,
	enumEntryData: null,
}

function compilerSpan() { return { file: '<compiler>', from: 0, to: 0 } }

function declareBuiltins() {
	const strr = struct('string', [
		param('buffer', { ...typeMap.pointer, to: typeMap.char }),
		param('length', typeMap.int)
	])
	addSymbol('string', strr)

	const arr = struct('array', [
		param('buffer', { ...typeMap.pointer, to: typeMap.void }),
		param('length', typeMap.int)
	])
	addSymbol('array', arr)

	// put scopes in the typemap so 'property access' knows about them
	typeMap.string.scope = strr.scope
	typeMap.array.scope = arr.scope

	const intData = struct('intData', [
		param('signed', typeMap.bool),
	])
	addSymbol('intData', intData)

	// set to typeinfo later on..
	const typePtr = cloneType(typeMap.pointer)

	state.pointerData = struct('pointerData', [
		param('to', typePtr),
	])
	addSymbol('pointerData', state.pointerData)

	const arrayData = struct('arrayData', [
		param('of', typePtr),
		param('count', typeMap.int),
	])
	addSymbol('arrayData', arrayData)

	const fieldArray = cloneType(typeMap.array)
	state.structData = struct('structData', [
		param('fields', fieldArray),
	])
	addSymbol('structData', state.structData)

	const entryArray = cloneType(typeMap.array)
	state.enumData = struct('enumData', [
		param('entries', entryArray),
	])
	addSymbol('enumData', state.enumData)

	const typeInfoData = union('data', [
		param('intData', intData),
		param('arrayData', arrayData),
		param('structData', state.structData),
		param('pointerData', state.pointerData),
		param('enumData', state.enumData),
		param('nothing', typeMap.void),
	])
	addSymbol('type info data', typeInfoData)

	typeInfo = struct('type info', [
		param('kind', typeMap.int),
		param('name', typeMap.string),
		param('size', typeMap.int),
		param('data', typeInfoData),
	])
	addSymbol('type info', typeInfo)

	state.fieldData = struct('fieldData', [
		param('name', typeMap.string),
		param('offset', typeMap.int),
		param('type', typePtr),
	])
	addSymbol('fieldData', state.fieldData)

	state.enumEntryData = struct('enumEntryData', [
		param('name', typeMap.string),
		param('tag', typeMap.int),
		// holds data for this specific enum entry
		// only used for tagged unions; fields.length is 0 for primitive enums
		param('fields', fieldArray),
	])
	addSymbol('enumEntryData', state.enumEntryData)

	typePtr.to = typeInfo
	fieldArray.of = state.fieldData
	entryArray.of = state.enumEntryData

	$typeof = fn('typeof', [param('symbol')], typeInfo)
	addSymbol('typeof', $typeof)

	$sizeof = fn('sizeof', [param('symbol')], typeMap.void)
	addSymbol('sizeof', $sizeof)


	// struct any [
	//     data:~>void,
	//     type:~>type info
	// ]

	const voidPtr = cloneType(typeMap.pointer)
	voidPtr.to = typeMap.void

	state.any = struct('any', [
		param('data', voidPtr),
		param('type', typePtr)
	])
	addSymbol('any', state.any)

	// add type infos
	for (let [name, type] of Object.entries(typeMap)) {
		if (type.tag == tag_array || type.tag == tag_struct || type.tag == tag_pointer) {
			// these are unique to each instance! skip for now and add when needed
			continue
		}

		// TODO: add additional type data
		const args = [num(type.tag, typeMap.int), str(name, typeMap.string), num(type.size, typeMap.int)]

		if (type.tag == tag_int) {
			args.push(ctor(intData, bool(type.signed, typeMap.bool)))
		}

		const t = ctor(typeInfo, ...args)
		const infoName = typeInfoLabel(type)
		const decl = MARK('const',)(declareVar(infoName, t))
		addSymbol(infoName, decl)
		// NOTE: as of right now we need the actual declaration for it to be emitted
		// TODO: perhaps just read the symbol table instead..
		state.ast.push(decl)
	}
}

function currentScope() { return state.scopeStack[state.scopeStack.length - 1]; }

function pushScope(scope, name, kind) {

	if (!scope) scope = {
		name,
		kind,
		parent: currentScope(),
		symbols: new Map(),
		used: new Set(),
	}

	state.scopeStack.push(scope)

	return scope
}

function popScope() {
	assert(state.scopeStack.length > 1, `scope stack not empty`)
	return state.scopeStack.pop()
}


function typeInfoFor(type) {
	const name = type.type
	const infoName = typeInfoLabel(type)
	let info = findSymbol(infoName, state.globalScope, false)

	if (!info) {
		assert(type.type == 'pointer' || type.type == 'array' || type.kind == 'struct' || type.kind == 'enum')

		// generate type info
		const args = [num(type.tag, typeMap.int), str(name, typeMap.string), num(type.size, typeMap.int)]

		if (type.tag == tag_enum) {


			// - emit entries (enumEntry{name:string,tag:int})
			// - push enumData onto the end of args (enumData{ entries:[]enumEntry })
			function emitEntry(entry) {

				function emitField(field) {
					const type = findSymbol(typeInfoLabel(field.type), state.globalScope, false)
					if (!type) console.log(typeInfoLabel(field.type))
					assert(type)

					const f = ctor(state.fieldData,
						str(field.name, typeMap.string),
						num(field.offset, typeMap.int),
						type
					)

					return f
				}

				const fieldsType = cloneType(typeMap.array)
				fieldsType.count = 0
				fieldsType.of = state.fieldData

				let fieldArr = {
					kind: 'arrayLiteral',
					entries: [],
					type: fieldsType,
					span: compilerSpan()
				}

				if (type.backingType.kind == 'struct') {
					const fields = entry.params.map(f => emitField(f))
					fieldsType.count = fields.length
					fieldArr.entries = fields
				}

				const f = ctor(state.enumEntryData,
					str(entry.name, typeMap.string),
					num(entry.value, typeMap.int),
					fieldArr,
				)
				return f
			}

			const entries = type.entries.map(emitEntry)
			const entriesType = cloneType(typeMap.array)
			entriesType.count = entries.length
			entriesType.of = state.enumEntryData

			const entryArr = {
				kind: 'arrayLiteral',
				entries: entries,
				type: entriesType,
				span: compilerSpan()
			}

			args.push(ctor(state.enumData, entryArr))
		}
		else if (type.tag == tag_pointer) {
			const to = findSymbol(typeInfoLabel(type.to), state.globalScope, false)
			// TODO: turn type info into a tagged union and support rtti for tagged unions
			if (type.to.name == 'type info') {
				args.push(ctor(state.pointerData, num(0, typeMap.int)))
			} else {
				assert(to)
				// args.push(ctor(pointerData, unary('->', to, ptr)))
				args.push(ctor(state.pointerData, to))
			}
		} else if (type.tag == tag_array) {
			const of = typeInfoFor(type.of) //findSymbol(typeInfoLabel(type.of), globalScope, false)
			// TODO: support nested arrays
			assert(of)
			const count = num(type.count || 0, typeMap.int)

			args.push(ctor(arrayData, of, count))
		} else if (type.tag == tag_struct) {
			function emitField(structLabel, field) {
				// TODO: rewrite this so it stores pointer to reference of type
				// (currently it just stored the declarevar directly)

				const type = typeInfoFor(field.type)//findSymbol(typeInfoLabel(field.type), globalScope, false)
				if (!type) console.log(typeInfoLabel(field.type))
				assert(type)

				const f = ctor(state.fieldData,
					str(field.name, typeMap.string),
					num(field.offset, typeMap.int),
					type
				)

				// const decl = MARK('const',)(declareVar(fieldName, f))
				// addSymbol(fieldName, decl)
				// NOTE: as of right now we need the actual declaration for it to be emitted
				// TODO: perhaps just read the symbol table instead..
				// ast.push(decl)

				return f
			}
			const fields = type.fields.map(f => emitField(infoName, f))
			const fieldsType = cloneType(typeMap.array)
			fieldsType.count = fields.length
			fieldsType.of = state.fieldData

			const fieldArr = {
				kind: 'arrayLiteral',
				entries: fields,
				type: fieldsType,
				span: compilerSpan()
			}

			args.push(ctor(state.structData, fieldArr))
		} else {
			console.log(type)
			assert(false)
		}

		const t = ctor(typeInfo, ...args)
		// already declared above
		// const infoName = typeInfoLabel(type)
		const decl = MARK('const',)(declareVar(infoName, t))
		addSymbol(infoName, decl)
		// NOTE: as of right now we need the actual declaration for it to be emitted
		// TODO: perhaps just read the symbol table instead..
		state.ast.push(decl)
		info = decl
	}

	assert(info)

	return info
}

function addSymbol(name, symbol) {
	currentScope().symbols.set(name, symbol)
}

function findSymbol(name, scope = null, recurse = true) {
	if (!scope) scope = currentScope()

	let symbol = scope.symbols.get(name)
	if (symbol) return symbol;
	for (let u of scope.used) {
		if (u.name == name) return u
		if (u.scope && recurse) {
			symbol = findSymbol(name, u.scope, false)
			if (symbol) return symbol
		}
	}

	if (!recurse) return false

	while (scope.parent && scope.kind != 'file') {
		// skip symbols in the scope above 'do' blocks
		if (scope.kind == 'do') {
			scope = scope.parent
			continue
		}
		return findSymbol(name, scope.parent)
	}

	// we reached the root
	// try find symbol at the top level of *other* file scopes
	if (scope.kind == 'file') {
		assert(scope.parent && scope.parent.kind == 'global')

		const globalScope = scope.parent
		symbol = findSymbol(name, globalScope, false)
		if (symbol) return symbol

		for (let file of state.fileScopes) {
			if (file == scope) continue
			symbol = findSymbol(name, file, false)
			if (symbol) return symbol
		}
	}

	return null
}

function typeEqual(a, b) {
	if (a.type != b.type) return false
	if (a.type == 'array') {
		// TODO: factor in capacity
		return typeEqual(a.of, b.of)
	} else if (a.type == 'pointer') {
		return typeEqual(a.to, b.to)
	} else {
		return true
	}
}

function coerceType(type, it) {
	if (typeEqual(type, it.type)) return it

	if (it.kind == 'numberLiteral') {
		if (type.tag == tag_float || type.tag == tag_int) {
			it.type = type
		}
	}

	// string literal -> char literal
	if (it.kind == 'stringLiteral') {
		if (type.type == 'char' && it.type.type == 'string') {
			if (it.len != 1) {
				console.log(it)
			}
			assert(it.len == 1)
			it.type = typeMap.char
		}
	}

	// implicit *T -> ~>void cast
	if (type.type == 'pointer' && type.to.type == 'void' && it.type.type == 'pointer') {
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	// implicit T[] -> any[] cast
	if (type.type == 'array' && type.of.type == 'any' && it.type.type == 'array') {

		// cast entries to any
		for (let i = 0; i < it.entries.length; i++) {
			const entry = it.entries[i]
			const castedEntry = { kind: 'implicit cast', type: type.of, expr: entry, span: entry.span }
			it.entries[i] = castedEntry
		}

		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	// implicit []char -> string cast
	if (type.type == 'string', it.type.type == 'array' && it.type.of.type == 'char') {
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	// implicit cstring -> ~>char AND string -> ~>void cast
	// implicit string -> ~>char AND string -> ~>void cast
	if (type.type == 'pointer' && (it.type.type == 'cstring' || it.type.type == 'string')) {
		const toType = type.to.type
		if (toType == 'void' || toType == 'char') {
			const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
			return cast
		}
	}

	// implicit []foo -> ~>foo cast AND []foo -> ~>void cast
	if (type.type == 'pointer' && it.type.type == 'array') {
		if (type.to.type == it.type.of.type || type.to.type == 'void') {
			const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
			return cast
		}
	}

	// TODO: introduce explicit cast
	if (type.type == 'int' && it.type.type == 'char') {
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	// TODO: introduce explicit cast
	if (it.type.type == 'void') {
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	if (type.kind == 'union') {
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	const intTypes = new Set(['u64', 'i64', 'int', 'uint'])

	if (intTypes.has(type.type) && intTypes.has(it.type.type)) {
		assert(type.size == it.type.size)
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	if (type.type == 'any' && it.type.type != 'any') {
		const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
		return cast
	}

	if (type.kind == 'enum' && it.kind == 'reference' && it.symbol.kind == 'enum alias') {
		assert(it.symbol.originalType)
		if (typeEqual(type, it.symbol.originalType)) {
			// downcast back to the base type
			const cast = { kind: 'implicit cast', type: type, expr: it, span: it.span }
			return cast
		}
	}

	if (type.tag == tag_pointer && it.kind == 'nullLiteral') {
		it.type = type
	}

	const equal = typeEqual(type, it.type)

	if (!equal) {
		console.log("expected:")
		console.log(type)
		console.log("got:")
		console.log(it)
	}
	assert(equal, 'it matches type after coersion')

	return it
	// if (!type) return it

	// const intTypes = new Set(['u64', 'i64', 'int', 'uint'])

	// if (intTypes.has(type) && intTypes.has(it.type.type)) {
	//     assert(type.size == it.type.size)
	//     const cast = { kind: 'implicit cast', type, expr: it }
	//     return cast
	// }

	// if (it.type.type == 'void' && type) {
	//     const cast = { kind: 'implicit cast', type: type, expr: it }
	//     return cast
	// }

	// assert(it.type.type == type.type, `type matches ${it.type.type} ${type.type}`)
}
function bind(files) {

	/*
	our compiler binds declarations in passes:
	- pass 0 binds types, structs, globals, function signatures etc.
	- pass 1 binds using statements, function bodies etc.

	we do this because if we try to bind a using statement in pass 0 the module may not exist yet
	this isn't perfect since functions may still refer to custom types in their signature;
	in this case the struct should be declared *before* the function

	eventually we should rewrite the pass system to be truly order-independent but it works for now
	*/
	let pass = 0
	const bodies = new Map()
	const usings = []
	state.ast = []
	state.scopeStack = []
	state.fileScopes = []

	state.globalScope = pushScope(null, 'root', 'global')

	declareBuiltins()

	/*********************************
	 *            pass 0             *
	 *********************************/

	// TODO: proper order-independent lookups
	// pretty sure reverse is still a good idea for performance (becaue imports don't rely on main, but main relies on imports)
	for (let root of [...files].reverse()) {
		assert(root.kind == 'file')
		// bind declarations in file
		bindFile(root)
	}

	/*********************************
	 *            pass 1             *
	 *********************************/
	pass = 1

	for (let [scope, node, it] of usings) {
		pushScope(scope)
		const usedScope = findSymbol(node.path.value)
		popScope()
		assert(usedScope, `scope is defined '${node.path.value}'`)
		scope.used.add(usedScope)
		it.usedScope = usedScope
	}
	// bind declaration bodies
	for (let [symbol, body] of bodies) {
		bindBody(body, symbol)
	}

	return state.ast



	function bindFile(node) {
		state.fileScopes.push(pushScope(null, 'file', 'file'))
		for (let decl of node.declarations) {
			const it = bindDeclaration(decl)
			assert(it)
			state.ast.push(it)
		}
		popScope()
	}

	function createEnumAlias(entry, name) {
		const s = struct(entry.name + '$backingStruct', entry.params)
		aliasType = s
		const alias = {
			kind: 'enum alias',
			name: name?.value ?? 'it',
			type: s,
			span: name?.span ?? compilerSpan(),
			originalType: entry.type
		}

		addSymbol(alias.name, alias)

		return alias
	}

	function bindBody(node, symbol) {
		switch (node.kind) {
			case 'proc': {
				pushScope(symbol.scope)
				symbol.instructions = bindBlock(node.body)
				popScope()
				return
			}
			default:
				assert(false, `unhandled kind "${node.kind}"`)
		}
	}

	function bindDeclaration(node) {
		switch (node.kind) {
			case 'import': {
				return { kind: 'import', path: node.path }
			}
			case 'use': {
				assert(node.path.kind == 'symbol')

				if (pass == 0) {
					// modules may not yet be bound, delay binding
					const it = { kind: 'use' }
					usings.push([currentScope(), node, it])
					return it
				}

				const it = { kind: 'use' }

				const usedScope = findSymbol(node.path.value)
				assert(usedScope, `scope is defined '${node.path.value}'`)
				currentScope().used.add(usedScope)
				it.usedScope = usedScope

				return it
			}
			case 'module': {
				const it = { kind: 'module', name: node.name.value, declarations: undefined }
				addSymbol(it.name, it)
				it.scope = pushScope(null, it.name)
				it.declarations = bindBlock(node.block)
				popScope()
				return it
			}
			case 'type alias': {
				const name = node.name.value
				assert(name)
				const of = bindType(node.type)
				assert(of)
				// TODO: unhack type alias
				assert(!typeMap[name], `type name is unique`)
				typeMap[name] = of
				const it = { kind: 'type alias', name, of }
				return it
			}
			case 'var': {
				assert(
					!currentScope().symbols.has(node.name.value),
					`${node.name.value} is a unique name`
				)
				const it = {
					kind: 'declareVar',
					name: node.name.value,
					expr: undefined,
					notes: new Map(),
					scope: currentScope()
				}
				const isConst = node.keyword.value == 'const'
				if (isConst) it.notes.set('const', [])


				if (node.type) {
					it.type = bindType(node.type)
				}

				if (node.expr) {
					it.expr = bindExpression(node.expr)
					assert(it.expr.type, `expressions must have a type`)
					if (!it.type) it.type = it.expr.type
				}

				// NOTE: add symbol *after* binding the expression since we don't want the expression to refer to this symbol
				// this allows us to refer to variables of the same name that were declared earlier in the program
				addSymbol(it.name, it)

				assert(it.type, 'type must be either provided or inferred from expression')

				if (it.expr) {
					it.expr = coerceType(it.type, it.expr)
				}

				return it
			}
			case 'enum': {
				// TODO: better way to calculate enum value
				let i = 0

				let fields = []

				function bindEnumEntry(node) {
					assert(node.kind == 'enum entry')
					const name = node.name.value
					const scope = pushScope()
					let params = []
					if (node.params && node.params.items.length) {
						params = bindParameters(node.params)

						let offset = 0
						for (let param of params) {
							param.offset = offset
							assert(param.type.size)
							offset += roundToIncrement(param.type.size, 8)
							fields.push(param)
						}
					}
					popScope()

					let value
					if (node.value) {
						assert(node.value.kind == 'number')
						value = node.value.value
					} else {
						value = i++
					}

					const it = {
						kind: 'enum entry',
						name,
						value,
						params,
						scope,
						notes: new Map(),
						span: node.name.span
					}
					addSymbol(name, it)
					return it
				}

				const name = node.name.value
				assert(name)
				assert(!findSymbol(name, null, false), 'name is unique')

				const it = {
					tag: tag_enum,
					kind: 'enum',
					type: name,
					name,
					scope: null,
					sharedFields: null,
					entries: null,
					backingType: null,
					size: null,//backingType.size,
					notes: new Map()
				}


				addSymbol(name, it)

				const scope = pushScope(null, name, 'enum')

				let backingType = typeMap.int
				let offset = backingType.size
				let sharedFields = bindParameters(node.params)
				for (let field of sharedFields) {
					field.offset = offset
					assert(field.type.size)
					offset += roundToIncrement(field.type.size, 8)
				}


				const entries = bindList(node.entries.items, bindEnumEntry)
				if (fields.length || sharedFields.length) {
					// update the field offsets to account for the tag
					for (let f of fields) {
						f.offset += offset
					}

					// put tag and shared fields at the start of fields
					fields = [param('tag', backingType), ...sharedFields, ...fields]

					// update the backing type so it's a struct containing the tag followed by the fields
					backingType = struct(name + '$backingTaggedUnion', fields)
				}

				popScope(scope)

				entries.forEach(v => {
					v.type = it
				})

				for (let n of node.tags) {
					it.notes.set(...bindTag(n))
				}

				it.scope = scope
				it.sharedFields = sharedFields
				it.entries = entries
				it.backingType = backingType
				it.size = backingType.size

				return it
			}
			case 'union':
			case 'struct': {
				const kind = node.kind
				assert(kind == 'struct' || kind == 'union')

				const scope = pushScope(null, node.name.value, kind)
				const name = node.name.value
				const it = {
					tag: tag_struct,
					kind,
					name: name,
					type: name,
					fields: bindParameters(node.parameters),
					scope,
					notes: new Map(),
					span: spanFromRange(node.keyword.span, node.parameters.end.span)
				}
				for (let n of node.tags) {
					it.notes.set(...bindTag(n))
				}

				popScope(scope)

				addSymbol(name, it)

				it.size = 0

				if (kind == 'struct') {
					for (let field of it.fields) {
						assert(field.type)
						assert(field.type.size)
						field.kind = 'field'
						// TODO: alignment
						assert(field.type.size % 8 == 0)
						field.offset = it.size
						it.size += field.type.size
					}
				} else {
					assert(it.kind == 'union')
					for (let field of it.fields) {
						assert(field.type)
						assert(field.type.size)
						field.kind = 'field'
						assert(field.type.size % 8 == 0)
						field.offset = 0
						it.size = Math.max(it.size, field.type.size)
					}
				}

				assert(it.size > 0)

				return it
			}
			case 'do': {
				const it = {
					kind: 'do',
					name: node.name.value,
					instructions: undefined,
					notes: new Map()
				}
				for (let n of node.tags) {
					it.notes.set(...bindTag(n))
				}

				it.scope = pushScope(null, it.name, 'do')
				it.params = bindScopeParameters(node.parameters)
				it.instructions = bindBlock(node.body)
				popScope()

				return it
			}
			case 'proc': {
				assert(
					!currentScope().symbols.has(node.name.value),
					`${node.name.value} is a unique name`
				)

				const returnType = node.returnType
					? bindType(node.returnType)
					: typeMap.void

				const it = {
					kind: 'function',
					name: node.name.value,
					instructions: undefined,
					returnType,
					notes: new Map()
				}


				for (let n of node.tags) {
					it.notes.set(...bindTag(n))
				}

				addSymbol(it.name, it)
				it.scope = pushScope(null, it.name)
				it.params = bindParameters(node.parameters)
				popScope()

				if (node.body && node.body.kind == 'block') bodies.set(it, node)
				else it.instructions = null

				return it
			}
			case 'match': {
				const operand = bindExpression(node.operand)
				const isEnum = operand.type.kind == 'enum'

				const arms = []
				const checkedValues = new Set()
				for (let arm of node.arms) {
					if (isEnum) assert(arm.pattern.kind == 'pattern equal')

					let pattern, scope

					switch (arm.pattern.kind) {
						case 'pattern equal': {
							if (arm.pattern.symbol.value == 'else') {
								const isLast = node.arms[node.arms.length - 1] == arm
								assert(isLast, `else pattern is only used at the end of a match block`)
								pattern = { kind: 'pattern else' }
							} else {
								const value = findSymbol(arm.pattern.symbol.value, operand.type.scope, false)
								scope = pushScope()
								if (!value) console.log(arm.pattern)
								assert(value)
								if (checkedValues.has(value)) {
									assert(false, `duplicate entry ${arm.pattern.symbol.value}, ${JSON.stringify(arm.pattern.symbol.span)}`)
								}
								checkedValues.add(value)

								let alias
								if (value.params.length) {
									alias = createEnumAlias(value)
								}
								pattern = {
									kind: 'pattern equal',
									value,
									alias,
									scope,
									span: arm.pattern.span
								}
							}
						} break
						case 'pattern expression': {
							let expr = bindExpression(arm.pattern.expr)
							if (expr.kind == 'stringLiteral') {
								expr = coerceType(operand.type, expr)
								assert(expr.type.type == 'char', `match only supports char literals`)
								pattern = {
									kind: 'pattern value',
									value: expr,
									span: expr.span
								}
							} else {
								assert(expr.type.type == 'bool')
								pattern = {
									kind: 'pattern expression',
									expr,
									span: expr.span
								}
							}
						} break
						default:
							throw `unknown arm pattern ${arm.pattern.kind}`
							break;
					}

					const body = bindDeclaration(arm.block)
					let span = spanFromRange(arm.pattern.span, body.span)

					if (scope) popScope()

					const it = {
						kind: 'arm',
						pattern,
						body,
						span,
					}
					arms.push(it)
				}

				if (isEnum) {
					for (let entry of operand.type.entries) {
						if (!checkedValues.has(entry)) {
							assert(false, `unhandled enum entry ${entry.name} at ${JSON.stringify(node.keyword.span)}`)
						}
					}
				}

				const span = spanFromRange(node.keyword.span, node.end.span)
				const it = {
					kind: 'match',
					operand,
					arms,
					span
				}
				return it
			}
			case 'if': {
				pushScope()
				const cond = bindExpression(node.condition)
				const then = node.thenBlock ? bindDeclaration(node.thenBlock) : null
				popScope()

				let els = null
				if (node.elseBlock) {
					pushScope()
					els = bindDeclaration(node.elseBlock)
					popScope()
				}

				const it = {
					kind: 'if',
					cond,
					then,
					els
				}
				return it
			}
			case 'goto': {
				// TODO: stop recursing at the function scope (don't allow labels across stack bounds)
				const label = findSymbol(node.label.value)
				assert(label, `label is defined`)
				const hasCondition = node.condition
				let condition
				if (hasCondition) {
					condition = bindExpression(node.condition)
					assert(condition.type.type == 'bool')
				}
				const it = {
					kind: 'goto',
					label,
					condition,
					span: spanFromRange(node.keyword.span, node.label.span) // todo: real span?
				}
				return it

			}
			case 'label': {
				const it = {
					kind: 'label',
					name: node.label.value,
					span: spanFromRange(node.keyword.span, node.colon.span)
				}
				addSymbol(it.name, it)
				return it
			}
			case 'while': {
				const scope = pushScope(null, null, 'while')

				const condition = bindExpression(node.condition)
				assert(condition.type.type == 'bool')
				const block = bindBlock(node.block)

				popScope()

				const it = {
					kind: 'while',
					scope,
					condition,
					block,
					span: spanFromRange(node.keyword.span, node.block.end.span)
				}
				return it
			}
			case 'for': {
				const scope = pushScope(null, null, 'for')

				const preCondition = node.preCondition ? bindDeclaration(node.preCondition) : null
				const condition = node.condition ? bindExpression(node.condition) : null
				const postCondition = node.postCondition ? bindExpression(node.postCondition) : null

				assert(condition.type.type == 'bool')

				const block = bindBlock(node.block)

				popScope()

				const it = {
					kind: 'for',
					scope,
					preCondition,
					condition,
					postCondition,
					block,
					span: spanFromRange(node.keyword.span, node.block.end.span)
				}
				return it
			}

			case 'each': {
				const scope = pushScope(null, null, 'each')

				assert(node.item.kind == 'symbol')
				const item = {
					kind: 'declareVar',
					name: node.item.value,
					expr: undefined,
					notes: new Map(),
					scope: currentScope()
				}

				addSymbol(item.name, item)

				let index
				if (node.index) {
					index = {
						kind: 'declareVar',
						name: node.index.value,
						expr: undefined,
						notes: new Map(),
						scope: currentScope(),
						type: typeMap.int
					}
					addSymbol(index.name, index)
				}

				assert(node.list.kind == 'symbol')
				const list = findSymbol(node.list.value)
				assert(list)

				const isInt = list.type.type == 'int'
				assert(isInt || list.type.type == 'array' || list.type.type == 'string' || list.kind == 'enum')
				if (list.kind == 'enum') {
					item.type = list
				} else if (list.type.type == 'array') {
					item.type = list.type.of
				} else if (list.type.type == 'string') {
					item.type = typeMap.char
				} else {
					assert(isInt)
					item.type = typeMap.int
				}

				const block = bindBlock(node.block)

				popScope()

				addSymbol(item.name, item)
				const it = {
					kind: 'each',
					item,
					index,
					list,
					block,
					scope,
					span: spanFromRange(node.keyword.span, node.block.end.span)
				}
				return it
			}
			case 'return': {
				const it = {
					kind: 'return',
					expr: node.expr ? bindExpression(node.expr) : null
				}
				return it
			}

			case 'break':
			case 'continue':
				return node

			case 'binary':
			case 'unary':
			case 'assignment':
			case 'call':
			case 'return':
			case 'pre unary':
			case 'post unary':
			case 'parenthesized expression':
			case 'property access': {
				return bindExpression(node)
			}

			case 'terminated expression': {
				return bindExpression(node.expr)
			}
			case 'block': {
				return bindBlock(node)
			}
			default:
				console.log(node)
				throw node
				assert(false, `unhandled kind "${node.kind}"`)
		}
	}

	function getTypePath(name) {
		// type namespacing not supported yet!
		// just return the top for now
		while (name && name.kind != 'symbol') {
			assert(name.kind == 'property access')
			name = name.property
		}

		// TODO: get the full span
		return [name.value, name.span]
	}

	function bindType(node) {

		switch (node.kind) {
			case 'type atom': {
				const [name, span] = getTypePath(node.name)
				let type = typeMap[name]
				if (type) type = cloneType(type)
				else {
					type = findSymbol(name)
					if (!type) console.log(name)
					assert(type)
				}
				assert(type.type, `'${name}' is a legal type`)
				type.span = span
				return type
			}
			case 'type array': {
				const type = cloneType(typeMap.array)
				type.of = bindType(node.of)
				if (node.size) {
					if (node.size.kind == 'symbol') {
						const s = findSymbol(node.size.value)
						assert(s)
					} else {
						assert(node.size.kind == 'number')
						assert(node.size.value !== undefined)
						type.count = node.size.value
					}
				}
				type.span = spanFromRange(node.begin.span, type.of.span)
				return type
			}
			case 'type pointer': {
				const it = cloneType(typeMap.pointer)
				it.to = bindType(node.to)

				// allow direct access to members behin the pointer
				// e.g for *vec3 v: v.x = 10    (translates to (*v).x = 10)
				if (it.to.scope) {
					it.scope = it.to.scope
				}

				it.span = spanFromRange(node.pointer.span, it.to.span)
				return it
			}
			case 'type optional': {
				// as of right now there isn't much value in having a "optional" type wrapper
				// so just return the type with a flag set instead
				const it = bindType(node.it)
				it.optional = true
				return it
			}
			case 'type mutable': {
				// as of right now there isn't much value in having a "mutable" type wrapper
				// so just return the type with a flag set instead
				const it = bindType(node.it)
				it.mutable = true
				return it
			}
			case 'type function': {
				const it = cloneType(typeMap.function)
				it.params = node.params.items.map(bindType)
				it.returns = bindType(node.returnType)
				it.span = spanFromRange(node.params.begin.span, it.returns.span)
				return it
			}
			default:
				console.log(node)
				assert(false, `unhandled node kind`)
		}
		assert(false)
	}
	function bindTag(node) {
		const t = node.value
		let args = []
		if (node.list)
			args = node.list.items.filter((_, i) => i % 2 == 0).map(bindExpression)
		return [t, args]
	}

	function bindPossiblyNamedExpression(node, inScope) {
		// interpret x=foo as named argument
		const isNamedArgument = node.kind == 'assignment' && node.name.kind == 'symbol' && node.operator.value == '='

		if (!isNamedArgument) {
			return bindExpression(node, inScope)
		}

		const expr = bindExpression(node.expr, inScope)
		const it = {
			kind: 'named argument',
			name: node.name.value,
			expr,
			type: expr.type,
			span: expr.span,
		}
		return it
	}

	function bindExpression(node, inScope) {
		switch (node.kind) {
			case 'null literal': return { kind: 'nullLiteral', span: node.span, type: typeMap.null }
			case 'boolean literal':
				return { kind: 'booleanLiteral', value: node.value, span: node.span, type: typeMap.bool }
			case 'number':
				const isUnsigned = node.radix != 10 || node.value > 0xFF_FF_FF_FF
				if (node.floating) {
					return { kind: 'numberLiteral', n: node.value, type: cloneType(typeMap.f64), span: node.span }
				} else if (isUnsigned) {
					return { kind: 'numberLiteral', n: node.value, type: cloneType(typeMap.uint), span: node.span }
				} else {
					return { kind: 'numberLiteral', n: node.value, type: cloneType(typeMap.int), span: node.span }
				}
			case 'string':
				const v = new TextEncoder().encode(node.value)
				return {
					kind: 'stringLiteral',
					value: node.value,
					len: v.length,
					type: node.value.endsWith('\0') // TODO: base type on the c prefix
						? cloneType(typeMap.cstring)
						: cloneType(typeMap.string),
					span: node.span
				}
			case 'array literal': {
				assert(node.items.length > 0, `array literal expressions cannot be empty`)
				const it = {
					kind: 'arrayLiteral',
					entries: bindList(node.items, bindExpression),
					span: spanFromRange(node.begin, node.end),
					type: undefined
				}

				const entryType = it.entries.reduce((acc, cur) => {
					if (!acc) return cur.type
					if (acc.type == cur.type.type) return acc
					// type didn't match, this must be an any[]
					return any
				}, null)

				const arrayType = {
					tag: tag_array,
					type: 'array',
					count: it.entries.length,
					of: entryType,
					size: 16,
					span: compilerSpan()
				}

				it.type = arrayType

				for (let i = 0; i < it.entries.length; i++) {
					it.entries[i] = coerceType(entryType, it.entries[i])
					// assert(it.entries[i].type.type == entryType.type)
				}

				return it
			}
			case 'parenthesized expression': {
				const expr = bindExpression(node.expr);
				assert(expr.type)
				const type = expr.type
				return { kind: 'parenthesized expression', expr, type, span: spanFromRange(node.open.span, node.close.span) }
			}
			case 'post unary': {
				const it = {
					kind: 'postUnary',
					op: node.op.value,
					expr: bindExpression(node.expr),
				}
				it.type = it.expr.type
				it.span = spanFromRange(it.expr.span, node.op.span)
				return it
			}
			case 'pre unary': {
				const it = {
					kind: 'preUnary',
					op: node.op.value,
					expr: bindExpression(node.expr),
				}
				if (it.op == '<~') {
					assert(it.expr.type.type == 'pointer')
					assert(it.expr.type.to)
					it.type = it.expr.type.to
				} else if (it.op == '~>') {
					it.type = cloneType(typeMap.pointer)
					it.type.to = it.expr.type
				} else {
					it.type = it.expr.type
				}

				if (it.op == '!') {
					const isUInt = it.expr.type.tag == tag_int && !it.expr.type.signed
					const isBool = it.expr.type.tag == tag_bool
					assert(isBool || isUInt, "operator'!' is only valid for unsigned integers or booleans")
				}

				if (it.op == '-') {
					const isSInt = it.expr.type.tag == tag_int && it.expr.type.signed
					assert(isSInt, "operator'-' is only valid for signed integer types")
				}

				it.span = spanFromRange(it.expr.span, node.op.span)
				return it
			}
			case 'symbol': {
				const symbol = findSymbol(node.value, inScope)
				if (!symbol) {
					console.log("SCOPE:")
					console.log(inScope)
					console.log("NODE:")
					console.log(node)
					throw '?'
				}
				assert(symbol, `symbol "${node.value}" is defined`)
				const type = symbol.type
				const it = {
					kind: 'reference',
					symbol,
					type,
					span: node.span
				}


				return it
			}
			case 'cast': {
				const expr = bindExpression(node.expr)
				const type = bindType(node.type)
				const span = node.span
				const it = {
					kind: 'cast',
					expr,
					type,
					span,
				}
				return it
			}
			case 'reinterpret': {
				const expr = bindExpression(node.expr)
				const type = bindType(node.type)
				assert(expr.type.size == type.size, `reinterpet is only valid for types of the same size ${expr.type.size}, ${type.size}`)
				const span = node.span
				const it = {
					kind: 'reinterpret',
					expr,
					type,
					span,
				}
				return it
			}
			case 'offset access': {
				const left = bindExpression(node.name)
				assert(left)
				const legalTypes = ['array', 'string', 'cstring', 'pointer']
				assert(legalTypes.includes(left.symbol.type.type), `can only access offset of arrays, strings,cstrings, pointers`)

				const index = bindExpression(node.index)

				const it = {
					kind: 'indexedAccess',
					left,
					index,
					span: spanFromRange(left, node.end.span)
				}
				if (left.symbol.type.type == 'pointer') {
					it.type = left.symbol.type.to
				} else if (left.symbol.type.type == 'array') {
					it.type = left.symbol.type.of
				} else {
					assert(left.symbol.type.type == 'cstring' || left.symbol.type.type == 'string')
					it.type = typeMap.char
				}

				return it
			}
			case 'property access': {
				const left = bindExpression(node.scope, inScope)
				assert(left)

				function checkIfLhsIsLegal(left) {
					if ((left.kind == 'readProp' || left.kind == 'reference') && left.symbol?.type?.type == 'pointer') {
						const to = left.symbol.type.to
						assert(to.kind == 'struct' || to.kind == 'union' || to.type == 'string' || to.type == 'array')
					} else {
						const isLegal = left.symbol?.type?.kind == 'struct' || left.symbol?.type?.kind == 'union' || left.symbol?.type?.type == 'string' || left.symbol?.type?.type == 'array' || left.symbol.scope
						if (!isLegal) {
							console.log(left)
						}
						assert(isLegal, `property access is allowed to be: module, struct, union, pointer`)
					}
				}
				checkIfLhsIsLegal(left)

				let scope

				if (left.symbol) {
					if (left.kind == 'reference' && left.symbol.kind == 'enum alias') {
						assert(left.symbol.type.scope)
						scope = left.symbol.type.scope
					}
					else if (left.kind == 'reference' && left.symbol.type?.type == 'pointer') {
						assert(left.symbol.type.to.scope)
						scope = left.symbol.type.to.scope
					} else if (left.symbol.type?.scope) {
						scope = left.symbol.type?.scope
					} else {
						scope = left.symbol?.scope
					}
				}

				const right = bindExpression(node.property, scope)

				// TODO: give functions a type
				if (right.symbol.kind != 'function' && right.symbol.kind != 'module') {
					if (!right.type) console.log(right)
					assert(right.type)
				}
				const type = right.type

				const it = {
					kind: 'readProp',
					left,
					prop: right,
					// the symbol this readprop represents
					// this makes it some others parts of the compiler don't have to care about
					// wether something is a ref<symbol> or ref<readprop>
					symbol: right.symbol,
					type,
					span: spanFromRange(left.span, right.span)
				}

				return it
			}
			case 'call': {
				const def = bindExpression(node.name, inScope)

				const isStruct = def.symbol.kind == 'struct'
				const isEnumCtor = def.symbol.kind == 'enum entry'
				const isLambda = def.symbol.kind != 'function' && def.symbol.type?.type == 'function'

				if (isEnumCtor) {
					assert(def.symbol.type.backingType.kind == 'struct')
				} else if (isLambda) {
					assert(def.symbol.type.returns)
				} else if (!isStruct) {
					assert(def.symbol.returnType)
				}

				const type = isStruct ? def.symbol : isEnumCtor ? def.symbol.type : def.symbol.returnType

				const isSyscall = def.symbol.notes.has('syscall')

				// NOTE: as of right now lambas only record parameter types, no notes or spreads
				if (isLambda) {
					const paramTypes = def.symbol.type.params
					const args = bindList(node.argumentList.items, bindExpression)
					assert(args.length == paramTypes.length)
					let i = 0
					for (let arg of args) {
						args[i] = coerceType(paramTypes[i], arg)
						i++
					}

					const it = {
						kind: 'call',
						def,
						args,
						type: def.symbol.type.returns,
						span: spanFromRange(node.name.span, node.argumentList.end.span)
					}

					assert(paramTypes.length == it.args.length)

					for (let i = 0; i < paramTypes.length; i++) {
						let arg = it.args[i]
						let paramType = paramTypes[i]

						assert(arg.type)
						assert(paramType)

						arg = coerceType(paramType, arg)
						it.args[i] = arg

						assert(typeEqual(paramType, arg.type))
					}

					return it
				}

				if (isEnumCtor) {
					const params = [...def.symbol.type.sharedFields, ...def.symbol.params]
					const it = {
						kind: 'enumctorcall',
						entry: def.symbol,
						args: bindList(node.argumentList.items, bindPossiblyNamedExpression),
						type,
						span: spanFromRange(node.name.span, node.argumentList.end.span)
					}
					it.args = validateArguments(it.args, params, node.name.span.file)
					return it
				}
				else if (isStruct) {
					const params = def.symbol.fields
					const it = {
						kind: 'ctorcall',
						args: bindList(node.argumentList.items, bindPossiblyNamedExpression),
						type,
						span: spanFromRange(node.name.span, node.argumentList.end.span)
					}
					it.args = validateArguments(it.args, params, node.name.span.file)
					return it
				} else if (isSyscall) {
					const params = def.symbol.params
					const it = {
						kind: 'syscall',
						code: def.symbol.notes.get('syscall')[0],
						args: bindList(node.argumentList.items, bindPossiblyNamedExpression),
						type,
						span: spanFromRange(node.name.span, node.argumentList.end.span)
					}
					it.args = validateArguments(it.args, params, node.name.span.file)
					return it
				} else {
					const params = def.symbol.params
					const isTypeof = node.name.value == 'typeof'
					const isSizeOf = node.name.value == 'sizeof'

					if (isSizeOf) {
						assert(node.argumentList.items.length == 1)
						const type = bindType(node.argumentList.items[0])
						const it = {
							kind: 'sizeof',
							arg: type,
							type: typeMap.int,
							span: spanFromRange(node.name.span, node.argumentList.end.span)
						}
						return it
					}

					const args = bindList(node.argumentList.items, bindPossiblyNamedExpression)


					if (isTypeof) {
						assert(args.length == 1 && args[0].type)
						const it = ref(typeInfoFor(args[0].type))
						const ptr = cloneType(typeMap.pointer)
						ptr.to = it.type
						it.type = ptr
						return it
					} else {
						const it = {
							kind: 'call',
							def,
							args,
							type,
							span: spanFromRange(node.name.span, node.argumentList.end.span)
						}

						it.args = validateArguments(it.args, params, node.name.span.file)

						return it
					}
				}
			}
			case 'binary': {
				const op = node.op.value

				// // fooEnum == bar:b
				// if (node.rhs.kind == 'alias') {
				// 	const a = bindExpression(node.lhs)
				// 	assert(op == '==', `alias can only be bound with operator == (got ${op})`)
				// 	assert(a.kind == 'reference', `left hand side of type check shound reference a symbol`)
				// 	assert(a.type.kind == 'enum', `left hand symbol should be an enum`)

				// 	const b = ((node) => {
				// 		const property = findSymbol(node.name.value, a.type.scope)
				// 		assert(property)
				// 		const alias = createEnumAlias(property, node.alias)
				// 		const r = ref(property)
				// 		r.alias = alias
				// 		return r
				// 	})(node.rhs)

				// 	const type = typeMap.bool
				// 	const it = { kind: 'binary', a, op, b, type, span: spanFromRange(a.span, b.span) }
				// 	return it
				// }


				const a = bindExpression(node.lhs)
				let b

				// fooEnum == bar
				if (a.type.kind == 'enum' && node.rhs.kind == 'symbol') {
					b = bindExpression(node.rhs, a.type.scope)

					// from this point on the lhs should be treated as entry on the rhs
					// so we create an alias
					assert(b.kind == 'reference' && b.symbol.kind == 'enum entry')
					let alias
					if (node.lhs.kind == 'symbol') {
						alias = createEnumAlias(b.symbol, node.lhs) // use name of lhs
					} else {
						alias = createEnumAlias(b.symbol) // use default name
					}
					addSymbol(alias.name, alias)

					b.alias = alias
				} else {
					b = bindExpression(node.rhs)
				}

				assert(a.type)

				// x => f => g
				if (op == '=>') {
					assert(b.kind == 'reference' && b.symbol.kind == 'function')
					const it = {
						kind: 'pipe',
						left: a,
						call: b,
						type: b.symbol.returnType,
						span: spanFromRange(a.span, b.span)
					}

					assert(node.rhs.kind == 'symbol')
					const args = validateArguments([a], b.symbol.params, node.rhs.span.file)
					assert(args.length == 1)
					it.left = args[0]

					return it
				}

				assert(b.type)

				// attempt to match up type a with b, inserting implicit casts where needed
				// returns the dominant type if there is one
				function resolveTypeDispute(a, b) {
					if (typeEqual(a, b)) {
						if (a.type.tag == tag_enum) {
							if (op != '==' && op != '!=') {
								assert(a.symbol.type.notes.has('bitfield'), 'enum arithmetic is only executed on enums marked as #bitfield')
							}
							return a.type.backingType
						}

						// already legal, no need to do other checks
						return
					}

					if (a.kind == 'stringLiteral' && (b.type.type == 'int' || b.type.type == 'char')) {
						assert(a.len === 1)
						a.type = typeMap.char
					}

					if (b.kind == 'stringLiteral' && (a.type.type == 'int' || a.type.type == 'char')) {
						assert(b.len === 1)
						b.type = typeMap.char
					}

					if (a.kind == 'numberLiteral' && (b.type.tag == tag_int)) {
						a.type = b.type
					}
					if (b.kind == 'numberLiteral' && (a.type.tag == tag_int)) {
						b.type = a.type
					}

					if (a.kind == 'numberLiteral' && (b.type.tag == tag_float)) {
						a.type = b.type
					}
					if (b.kind == 'numberLiteral' && (a.type.tag == tag_float)) {
						b.type = a.type
					}

					if (a.type.kind == 'enum' && b.type.type == 'int') {

						assert(a.kind == 'reference')
						assert((a.kind == 'readProp' && a.prop.kind == 'reference') || a.kind == 'reference')
						assert(a.symbol.type.notes.has('bitfield'), 'enum arithmetic is only executed on enums marked as #bitfield')
						// allow it

						return b.type
					}

					if (b.type.kind == 'enum' && a.type.type == 'int') {

						assert((b.kind == 'readProp' && b.prop.kind == 'reference') || b.kind == 'reference')
						assert(b.symbol.type.notes.has('bitfield'), 'enum arithmetic is only executed on enums marked as #bitfield')
						// allow it

						return a.type
					}

					if (a.type.type == 'pointer' && b.type.tag == tag_int || b.type.type == 'pointer' && a.type.tag == tag_int) {
						// HACK: allow pointer arithmetic
					}
					else if (a.type.type == 'char' && b.type.tag == 1 || b.type.type == 'char' && a.type.tag == tag_int) {
						// allow it
					}
					else if (a.type.tag == tag_int && b.type.tag == tag_int) {
						// both numbers, allow it
						// TODO: proper number resolution
					} else {
						const equal = typeEqual(a.type, b.type)
						if (!equal) {
							console.log(a)
							console.log(b)
						}
						assert(equal)
					}
				}

				let type = resolveTypeDispute(a, b)


				const logicalOperators = new Set(['>', '>=', '<', '<=', '==', '!='])
				const isLogical = logicalOperators.has(op)
				if (isLogical) type = cloneType(typeMap.bool)
				if (!type) type = a.type

				const it = { kind: 'binary', a, op, b, type, span: spanFromRange(a.span, b.span) }
				return it
			}
			case 'assignment': {
				const varDec = bindExpression(node.name, inScope)
				assert(varDec, `symbol "${node.name.value}" is defined`)

				let expr = bindExpression(node.expr)

				expr = coerceType(varDec.type, expr)

				const it = {
					kind: 'assignVar',
					op: node.operator.value,
					varDec,
					expr,
					span: spanFromRange(node.name.span, expr.span)
				}
				return it
			}
			case 'lambda': {
				const returnType = node.returnType
					? bindType(node.returnType)
					: typeMap.void

				const type = cloneType(typeMap.function)
				type.returns = returnType

				const it = {
					kind: 'lambda',
					instructions: undefined,
					type,
					returnType,
					notes: new Map(),
				}


				for (let n of node.tags) {
					it.notes.set(...bindTag(n))
				}

				addSymbol(it.name, it)
				it.scope = pushScope(null, it.name)
				it.params = bindParameters(node.parameters)

				type.params = it.params.map(p => p.type)

				assert(node.body.kind == 'block')
				it.instructions = bindBlock(node.body)

				popScope()

				it.span = spanFromRange(node.parameters.begin.span, node.body.end.span)

				return it
			}
			default:
				console.log(node)
				assert(false, `unhandled kind "${node.kind}"`)
		}
	}

	function validateArguments(args, params, callsite) {
		for (let i = 0; i < params.length; i++) {
			const param = params[i]

			for (let [note, context] of param.notes.entries()) {
				switch (note) {
					case 'callee span': {
						assert(context.length == 1)
						const targetParam = context[0]
						assert(targetParam.kind == 'reference' && targetParam.symbol.kind == 'parameter')
						const index = params.indexOf(targetParam.symbol)
						assert(index > -1)
						if (!args[index]) {
							// inject callee span
							const expr = args[i]
							assert(args[i])
							const sourcecode = fileMap.get(expr.span.file)
							assert(sourcecode)
							const exprText = sourcecode.slice(expr.span.from, expr.span.to)
							assert(exprText)
							args[index] = bindExpression({ kind: 'string', value: exprText })
						}
					} break
					case 'call site': {
						assert(context.length == 0)
						if (!args[i]) {
							// inject call site
							args[i] = bindExpression({ kind: 'string', value: callsite })
						}
					} break
					default: {
						console.log('unhandled notes?')
						console.log(param.notes)
						assert(false)
					} break
				}
			}

			// TODO: figure out a way to make this work with callee span, call site, etc.
			if (param.spread) {

				const targetType = param.type.of
				// NOTE: mutates args in place
				const remainingArgs = args.splice(i)
					.map(a => coerceType(targetType, a))

				const it = {
					kind: 'spread',
					args: remainingArgs,
					span: remainingArgs.length == 0 ? null : spanFromRange(remainingArgs[0].span, remainingArgs[remainingArgs.length - 1].span),
					type: param.type,
				}

				args.push(it)
			}

			assert(args[i], `argument supplied for each parameter`)

			let arg = args[i]

			if (arg.kind == 'named argument') {
				if (arg.name != param.name) {
					console.log(`its name should be '${param.name}', but got '${arg.name}' (${JSON.stringify(arg.span)})`)
				}
				assert(arg.name == param.name)
			}

			assert(param.type, `param has a type`)
			assert(arg.type, `arument has a type`)

			arg = coerceType(param.type, arg)
			args[i] = arg
		}

		assert(params.length == args.length)

		return args
	}

	function bindParameters(params) {
		if (!params) return []
		return params.items.filter((_, i) => i % 2 == 0).map((p, i, arr) => {
			const isLast = i == arr.length - 1

			const spread = !!p.spread

			if (spread && !isLast) {
				assert(false, `spread is only used on last parameter`)
			}

			const it = {
				kind: 'parameter',
				name: p.name.value,
				spread,
				type: bindType(p.type),
				notes: new Map(),
			}

			it.span = spanFromRange(spread ? p.spread.span : p.name.span, it.type.span)
			addSymbol(it.name, it)
			return [p, it]
		}).map(([p, it]) => {
			// tags may reference other parameters, so we bind them in a second pass
			for (let n of p.tags) {
				it.notes.set(...bindTag(n))
			}
			return it
		})
	}
	function bindScopeParameters(params) {
		if (!params) return []
		return params.items.filter((_, i) => i % 2 == 0).map(p => {
			const it = { kind: 'do parameter' }
			const name = p.name.value
			assert(name)
			const type = bindType(p.type)
			const symbol = findSymbol(name, currentScope().parent)
			assert(symbol)
			assert(type)
			assert(symbol.type)
			if (type.type != symbol.type.type) {
				console.log(type.type)
				console.log(symbol.type.type)
			}
			assert(type.type == symbol.type.type)
			it.symbol = symbol
			currentScope().symbols.set(name, symbol)
			it.span = spanFromRange(p.name.span, type.span)
			return it
		})
	}

	function bindList(list, binder) {
		if (!list) return []
		return list.filter((_, i) => i % 2 == 0).map((p) => {
			return binder(p)
		})
	}

	function bindBlock(body, isExpression = false) {
		let boundStatements = new Array(body.statements.length)

		// first pass: declarations
		for (let i in body.statements) {
			const stmt = body.statements[i]
			if (stmt.kind == 'label') {
				boundStatements[i] = bindDeclaration(stmt)
			}
		}

		// second pass: expressions
		for (let i in body.statements) {
			const stmt = body.statements[i]
			if (!boundStatements[i]) {
				boundStatements[i] = bindDeclaration(stmt)
			}
		}

		// blocks may not have brackets (for file-level modules)
		// TODO: proper span based on declaration spans
		const span = body.begin ? spanFromRange(body.begin.span, body.end.span) : spanFromRange(boundStatements[0], boundStatements[boundStatements.length - 1])
		return {
			kind: 'block',
			isExpression,
			statements: boundStatements,
			span
		}
	}
}

module.exports = { bind, typeInfoFor, state }
