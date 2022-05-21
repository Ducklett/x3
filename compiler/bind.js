const { copyFileSync } = require('fs')
const { type } = require('os')
const { assert, declareVar, num, binary, ref, readProp, unary, label, goto, assignVar, indexedAccess, nop, call, ctor, struct, param, fn, str, MARK, union, bool, roundToIncrement } = require('./compiler')
const { fileMap } = require('./parser')

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

let any, $typeof, typeInfo, $typeInfoFor

const typeMap = {
	'unknown': { tag: tag_void, type: 'unknown', size: 0 },
	'null': { tag: tag_pointer, type: 'null', size: 0 },
	'int': { tag: tag_int, type: 'int', size: 8, signed: true },
	'uint': { tag: tag_int, type: 'uint', size: 8, signed: false },
	'u64': { tag: tag_int, type: 'u64', size: 8, signed: false },
	'i64': { tag: tag_int, type: 'i64', size: 8, signed: true },
	'f64': { tag: tag_float, type: 'f64', size: 8, signed: true },
	'f32': { tag: tag_float, type: 'f32', size: 4, signed: true },
	'void': { tag: tag_void, type: 'void', size: 0 },
	'string': { tag: tag_string, type: 'string', size: 16 },  // *char, length
	'cstring': { tag: tag_pointer, type: 'cstring', size: 8 }, // *char
	'array': { tag: tag_array, type: 'array', size: 16 },    // *values,length
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

	if (type.type == 'array') {
		l.push('$')
		l.push(typeInfoLabel(type.of))
	}

	return l.join('')
}

function bind(files) {
	$typeInfoFor = typeInfoFor
	function compilerSpan() { return { file: '<compiler>', from: 0, to: 0 } }
	function spanFromRange(from, to) {
		return { ...from, to: to.to }
	}

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
	const ast = []
	const scopeStack = []
	const fileScopes = []


	const globalScope = pushScope(null, 'root', 'global')

	// ========== declare builtin functions and types ============

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

	const pointerData = struct('pointerData', [
		param('to', typePtr),
	])
	addSymbol('pointerData', pointerData)

	const arrayData = struct('arrayData', [
		param('of', typePtr),
		param('count', typeMap.int),
	])
	addSymbol('arrayData', arrayData)

	const fieldArray = cloneType(typeMap.array)
	const structData = struct('structData', [
		param('fields', fieldArray),
	])
	addSymbol('structData', structData)

	const entryArray = cloneType(typeMap.array)
	const enumData = struct('enumData', [
		param('entries', entryArray),
	])
	addSymbol('enumData', enumData)

	const typeInfoData = union('data', [
		param('intData', intData),
		param('arrayData', arrayData),
		param('structData', structData),
		param('pointerData', pointerData),
		param('enumData', enumData),
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

	const fieldData = struct('fieldData', [
		param('name', typeMap.string),
		param('offset', typeMap.int),
		param('type', typePtr),
	])
	addSymbol('fieldData', fieldData)

	const enumEntryData = struct('enumEntryData', [
		param('name', typeMap.string),
		param('tag', typeMap.int),
		// holds data for this specific enum entry
		// only used for tagged unions; fields.length is 0 for primitive enums
		param('fields', fieldArray),
	])
	addSymbol('enumEntryData', enumEntryData)

	typePtr.to = typeInfo
	fieldArray.of = fieldData
	entryArray.of = enumEntryData

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

	any = struct('any', [
		param('data', voidPtr),
		param('type', typePtr)
	])
	addSymbol('any', any)

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
		ast.push(decl)
	}

	// ============================================================

	function currentScope() { return scopeStack[scopeStack.length - 1]; }

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

	return ast

	function typeInfoFor(type) {
		const name = type.type
		const infoName = typeInfoLabel(type)
		let info = findSymbol(infoName, globalScope, false)

		if (!info) {
			assert(type.type == 'pointer' || type.type == 'array' || type.kind == 'struct' || type.kind == 'enum')

			// generate type info
			const args = [num(type.tag, typeMap.int), str(name, typeMap.string), num(type.size, typeMap.int)]

			if (type.tag == tag_enum) {


				// - emit entries (enumEntry{name:string,tag:int})
				// - push enumData onto the end of args (enumData{ entries:[]enumEntry })
				function emitEntry(entry) {

					function emitField(field) {
						const type = findSymbol(typeInfoLabel(field.type), globalScope, false)
						if (!type) console.log(typeInfoLabel(field.type))
						assert(type)

						const f = ctor(fieldData,
							str(field.name, typeMap.string),
							num(field.offset, typeMap.int),
							type
						)

						return f
					}

					const fieldsType = cloneType(typeMap.array)
					fieldsType.count = 0
					fieldsType.of = fieldData

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

					const f = ctor(enumEntryData,
						str(entry.name, typeMap.string),
						num(entry.value, typeMap.int),
						fieldArr,
					)
					return f
				}

				const entries = type.entries.map(emitEntry)
				const entriesType = cloneType(typeMap.array)
				entriesType.count = entries.length
				entriesType.of = enumEntryData

				const entryArr = {
					kind: 'arrayLiteral',
					entries: entries,
					type: entriesType,
					span: compilerSpan()
				}

				args.push(ctor(enumData, entryArr))
			}
			else if (type.tag == tag_pointer) {
				const to = findSymbol(typeInfoLabel(type.to), globalScope, false)
				// TODO: turn type info into a tagged union and support rtti for tagged unions
				if (type.to.name == 'type info') {
					args.push(ctor(pointerData, num(0, typeMap.int)))
				} else {
					assert(to)
					// args.push(ctor(pointerData, unary('->', to, ptr)))
					args.push(ctor(pointerData, to))
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

					const f = ctor(fieldData,
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
				fieldsType.of = fieldData

				const fieldArr = {
					kind: 'arrayLiteral',
					entries: fields,
					type: fieldsType,
					span: compilerSpan()
				}

				args.push(ctor(structData, fieldArr))
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
			ast.push(decl)
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

			for (let file of fileScopes) {
				if (file == scope) continue
				symbol = findSymbol(name, file, false)
				if (symbol) return symbol
			}
		}

		return null
	}

	function pushScope(scope, name, kind) {

		if (!scope) scope = {
			name,
			kind,
			parent: currentScope(),
			symbols: new Map(),
			used: new Set(),
		}

		scopeStack.push(scope)

		return scope
	}

	function popScope() {
		assert(scopeStack.length > 1, `scope stack not empty`)
		return scopeStack.pop()
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


	function bindFile(node) {
		fileScopes.push(pushScope(null, 'file', 'file'))
		for (let decl of node.declarations) {
			const it = bindDeclaration(decl)
			assert(it)
			ast.push(it)
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
							assert(a.symbol.type.notes.has('bitfield'), 'enum arithmetic is only executed on enums marked as #bitfield')
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

function lower(ast) {
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
				assert(node.arg.size !== undefined)

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
					const exprTypeInfo = ref($typeInfoFor(node.expr.type))
					// HACK: make type a pointer because globals are always emitted as pointers
					const ptr = cloneType(typeMap.pointer)
					ptr.to = exprTypeInfo.type
					exprTypeInfo.type = ptr

					const data = ctor(any, expr, exprTypeInfo)

					toReturn.push(data)
					return toReturn
				}

				if (node.expr.type.type == 'void') {
					// we allow dereferencing of void pointers when the expected type can be inferrect from context
					// to make this work we replace the void type with that of the implicit cast before emitting to asm
					node.expr.type = node.type
					return lowerNode(node.expr)
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

				return lowerNode(node.expr)
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
					function getByteCount(type) {
						if (type.type == 'array') {
							return type.count * getByteCount(type.of)

						} else {
							return type.size
						}
					}
					const count = getByteCount(node.type)
					assert(count > 0)

					// zero initialize
					node.expr = {
						kind: 'arrayLiteral',
						// when entries are null the x64 backend will zero initialize the memory block
						entries: null,
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

					assert(node.type.tag == tag_int || node.type.tag == tag_float)

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
				const backingType = node.type.backingType
				assert(backingType)
				const tagType = backingType.fields[0].type
				assert(tagType)
				const args = [num(node.entry.value, tagType), ...(node.args ?? [])]

				const c = ctor(backingType, ...args)
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
				if (node.type.size == 8 || asTag) {
					// NOTE: only returns the tag, enum instances with values are instead stored in 'enumctor'
					const numericRepresentation = num(node.value, node.type.backingType)
					return lowerNode(numericRepresentation)
				} else {
					console.log(node)
					throw 'h'
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
								const c = ctor(symbol.backingType, tag)
								return lowerNode(c)
							} else {
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

module.exports = { bind, lower }
