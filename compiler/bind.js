const { assert, declareVar, num, binary, ref, readProp, unary, label, goto, assignVar, offsetAccess, nop, call, ctor, struct, param, fn, str, MARK } = require('./compiler')
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

// TODO: actually have different int types
const typeMap = {
    'int': { tag: tag_int, type: 'int', size: 8 },
    'uint': { tag: tag_int, type: 'int', size: 8 },
    'u64': { tag: tag_int, type: 'int', size: 8 },
    'i64': { tag: tag_int, type: 'int', size: 8 },
    'void': { tag: tag_void, type: 'void', size: 0 },
    'string': { tag: tag_string, type: 'string', size: 16 },  // *char, length
    'cstring': { tag: tag_pointer, type: 'cstring', size: 8 }, // *char
    'array': { tag: tag_array, type: 'array', size: 16 },    // *values,length
    'char': { tag: tag_char, type: 'char', size: 1 },
    'bool': { tag: tag_bool, type: 'bool', size: 1 },
    'pointer': { tag: tag_pointer, type: 'pointer', size: 8, to: undefined },
}

function cloneType(t) {
    return { ...t }
}

function bind(files) {
    function compilerSpan() { return { file: '<compiler>', from: 0, to: 0 } }
    function spanFromRange(from, to) {
        return { ...from, to: to.to }
    }

    const bodies = new Map()
    const usings = []
    const ast = []
    const scopeStack = []
    const fileScopes = []


    const globalScope = pushScope(null, 'root', 'global')

    // ========== declare builtin functions and types ============

    const typeInfo = struct('type info', [
        param('kind', typeMap.int),
        param('name', typeMap.string),
        param('size', typeMap.int),
    ])
    addSymbol('type info', typeInfo)

    const $typeof = fn('typeof', [param('symbol')], typeInfo)
    addSymbol('typeof', $typeof)

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

    // add type infos
    for (let [name, type] of Object.entries(typeMap)) {
        // TODO: add additional type data
        const t = ctor(typeInfo, num(type.tag, typeMap.int), str(name, typeMap.string), num(type.size, typeMap.int))
        const infoName = name + '$typeinfo'
        const decl = MARK('const',)(declareVar(infoName, t))
        addSymbol(infoName, decl)
        // NOTE: as of right now we need the actual declaration for it to be emitted
        // TODO: perhaps just read the symbol table instead..
        ast.push(decl)
    }

    // ============================================================

    function currentScope() { return scopeStack[scopeStack.length - 1]; }

    // TODO: proper order-independent lookups
    // pretty sure reverse is still a good idea for performance (becaue imports don't rely on main, but main relies on imports)
    for (let root of [...files].reverse()) {
        assert(root.kind == 'file')
        // bind declarations in file
        bindFile(root)
    }

    for (let [scope, node, it] of usings) {
        pushScope(scope)
        const usedScope = findSymbol(node.path.value)
        popScope()

        assert(usedScope, `scope is defined`)
        scope.used.add(usedScope)
        it.usedScope = usedScope
    }
    // bind declaration bodies
    for (let [symbol, body] of bodies) {
        bindBody(body, symbol)
    }

    return ast

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
            // skip symbols in the scope above 'scope' blocks
            if (scope.kind == 'scope') {
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

    function bindFile(node) {
        fileScopes.push(pushScope(null, 'file', 'file'))
        for (let decl of node.declarations) {
            const it = bindDeclaration(decl)
            assert(it)
            ast.push(it)
        }
        popScope()
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
                const it = { kind: 'use' }
                usings.push([currentScope(), node, it])
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

                addSymbol(it.name, it)

                if (node.type) {
                    it.type = bindType(node.type)
                }

                if (node.expr) {
                    it.expr = bindExpression(node.expr)
                    assert(it.expr.type, `expressions must have a type`)
                    if (!it.type) it.type = it.expr.type
                }

                assert(it.type, 'type must be either provided or inferred from expression')
                // TODO: better type matching than just comparing the base type
                if (it.expr) assert(it.expr.type.type == it.type.type, `type matches ${it.expr.type.type} ${it.type.type}`)

                return it
            }
            case 'union':
            case 'struct': {
                const kind = node.kind
                assert(kind == 'struct' || kind == 'union')

                const scope = pushScope(null, node.name.value, kind)
                const name = node.name.value
                const it = {
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
            case 'scope': {
                const it = {
                    kind: 'scope',
                    name: node.name.value,
                    instructions: undefined,
                    notes: new Map()
                }
                for (let n of node.tags) {
                    it.notes.set(...bindTag(n))
                }

                it.scope = pushScope(null, it.name, 'scope')
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
            case 'if': {
                const it = {
                    kind: 'if',
                    cond: bindExpression(node.condition),
                    then: bindDeclaration(node.thenBlock),
                    els: node.elseBlock ? bindDeclaration(node.elseBlock) : null,
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

                assert(list.type.type == 'array' || list.type.type == 'string')
                if (list.type.type == 'array') {
                    item.type = list.type.of
                } else {
                    item.type = typeMap.char
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
                    assert(type)
                }
                assert(type.type, `'${name}' is a legal type`)
                type.span = span
                return type
            }
            case 'type array': {
                // HACK: string buffer type; reinterpret as string
                // assert(node.of.kind == 'type atom' && node.of.name.value == 'char')
                // const type = typeMap.string
                // type.span = spanFromRange(node.begin.span, node.of.name.span)
                const type = cloneType(typeMap.array)
                type.of = bindType(node.of)
                if (node.size) {
                    assert(node.size.kind == 'number')
                    assert(node.size.value !== undefined)
                    type.count = node.size.value
                }
                type.span = spanFromRange(node.begin.span, node.of.name.span)
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
            case 'type mutable': {
                // as of right now there isn't much value in having a "mutable" type wrapper
                // so just return the type with a flag set instead
                const it = bindType(node.it)
                it.mutable = true
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

    function bindExpression(node, inScope) {
        switch (node.kind) {
            case 'boolean literal':
                return { kind: 'booleanLiteral', value: node.value, span: node.span, type: typeMap.bool }
            case 'number':
                return { kind: 'numberLiteral', n: node.value, type: cloneType(typeMap.int), span: node.span }
            case 'string':
                return {
                    kind: 'stringLiteral',
                    value: node.value,
                    len: node.value.length,
                    type: node.value.endsWith('\0')
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

                const entryType = it.entries[0].type

                const arrayType = {
                    type: 'array',
                    count: it.entries.length,
                    of: entryType,
                    size: 16,
                    span: compilerSpan()
                }

                it.type = arrayType

                for (let i = 0; i < it.entries.length; i++) {
                    assert(it.entries[i].type.type == entryType.type)
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
                if (it.op == '*') {
                    assert(it.expr.type.type == 'pointer')
                    assert(it.expr.type.to)
                    it.type = it.expr.type.to
                } else if (it.op == '&') {
                    it.type = cloneType(typeMap.pointer)
                    it.type.to = it.expr.type
                } else {
                    it.type = it.expr.type
                }
                it.span = spanFromRange(it.expr.span, node.op.span)
                return it
            }
            case 'symbol': {
                // string length hack
                if (node.value == 'length') {
                    return { kind: 'string length', type: cloneType(typeMap.u64), span: node.span }
                }

                const symbol = findSymbol(node.value, inScope)
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
            case 'offset access': {
                const left = bindExpression(node.name)
                assert(left)
                const legalTypes = ['array', 'string', 'cstring', 'pointer']
                assert(legalTypes.includes(left.symbol.type.type), `can only access offset of arrays, strings,cstrings, pointers`)

                const index = bindExpression(node.index)

                const it = {
                    kind: 'offsetAccess',
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
                    if (left.kind == 'reference' && left.symbol?.type?.type == 'pointer') {
                        const to = left.symbol.type.to
                        assert(to.kind == 'struct' || to.kind == 'union' || to.type == 'string' || to.type == 'array')
                    } else {
                        const isLegal = left.symbol?.type?.kind == 'struct' || left.symbol?.type?.kind == 'union' || left.symbol.scope || node.property.value == 'length'
                        assert(isLegal, `property access is allowed to be: string length hack,module, struct, union, pointer`)
                    }
                }
                checkIfLhsIsLegal(left)

                const right = bindExpression(node.property, left.symbol?.type?.scope ?? left.symbol?.scope)
                assert(right.type)
                const type = right.type

                const it = {
                    kind: 'readProp',
                    left,
                    prop: right,
                    type,
                    span: spanFromRange(left.span, right.span)
                }

                return it
            }
            case 'call': {
                const def = bindExpression(node.name, inScope)

                const isStruct = def.symbol.kind == 'struct'

                if (!isStruct) assert(def.symbol.returnType)
                const type = isStruct ? def.symbol : def.symbol.returnType

                const isSyscall = def.symbol.notes.has('syscall')

                const params = isStruct ? def.symbol.fields : def.symbol.params;
                if (isStruct) {
                    const it = {
                        kind: 'ctorcall',
                        args: bindList(node.argumentList.items, bindExpression),
                        type,
                        span: spanFromRange(node.name.span, node.argumentList.end.span)
                    }
                    it.args = validateArguments(it.args, params, node.name.span.file)
                    return it
                } else if (isSyscall) {
                    const it = {
                        kind: 'syscall',
                        code: def.symbol.notes.get('syscall')[0],
                        args: bindList(node.argumentList.items, bindExpression),
                        type,
                        span: spanFromRange(node.name.span, node.argumentList.end.span)
                    }
                    it.args = validateArguments(it.args, params, node.name.span.file)
                    return it
                } else {
                    const isTypeof = node.name.value == 'typeof'

                    const it = {
                        kind: 'call',
                        def,
                        args: bindList(node.argumentList.items, bindExpression),
                        type,
                        span: spanFromRange(node.name.span, node.argumentList.end.span)
                    }

                    if (isTypeof) {
                        assert(it.args.length == 1 && it.args[0].type)
                        const to = it.type
                        const type = typeMap.pointer
                        type.to = to
                        it.type = type
                        assert(it.args[0].type.type != 'struct' && it.args[0].type.type != 'union', 'structs not yet supported')
                        const infoName = it.args[0].type.type + "$typeinfo"
                        // console.log(infoName)
                        const info = findSymbol(infoName, globalScope, false)
                        // console.log(globalScope)
                        assert(info)
                        it.info = info
                    } else {
                        it.args = validateArguments(it.args, params, node.name.span.file)
                    }

                    return it
                }
            }
            case 'binary': {
                const op = node.op.value

                const a = bindExpression(node.lhs)
                const b = bindExpression(node.rhs)
                assert(a.type)

                if (op == '->') {
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
                if (a.kind == 'stringLiteral' && (b.type.type == 'int' || b.type.type == 'char')) {
                    a.type = typeMap.char
                }
                if (b.kind == 'stringLiteral' && (a.type.type == 'int' || a.type.type == 'char')) {
                    b.type = typeMap.char
                }

                if (a.type.type == 'char' && b.type.type == 'int' || b.type.type == 'char' && a.type.type == 'int') {
                    // allow it
                } else {
                    assert(a.type.type == b.type.type)
                }


                const logicalOperators = new Set(['>', '>=', '<', '<=', '==', '!='])
                const isLogical = logicalOperators.has(op)
                const type = isLogical ? cloneType(typeMap.bool) : a.type
                const it = { kind: 'binary', a, op, b, type, span: spanFromRange(a.span, b.span) }
                return it
            }
            case 'assignment': {
                const varDec = bindExpression(node.name)
                assert(varDec, `symbol "${node.name.value}" is defined`)

                const expr = bindExpression(node.expr)

                // TODO: real type coersion
                if (varDec.type.type == 'char' && expr.kind == 'stringLiteral') {
                    assert(expr.len == 1)
                    expr.kind = 'charLiteral'
                    expr.type = cloneType(typeMap.char)
                }

                assert(varDec.type.type == expr.type.type)

                const it = {
                    kind: 'assignVar',
                    op: node.operator.value,
                    varDec,
                    expr,
                    span: spanFromRange(node.name.span, expr.span)
                }
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

            if (param.notes.size > 0) {
                // TODO: handle notes in switch and throw on unhandled note
                if (param.notes.has('callee span')) {
                    const context = param.notes.get('callee span')
                    assert(context.length == 1)
                    const targetParam = context[0]
                    assert(targetParam.kind == 'reference' && targetParam.symbol.kind == 'parameter')
                    const index = params.indexOf(targetParam.symbol)
                    assert(index > -1)
                    if (!args[index]) {
                        // inject callee span ;)
                        const expr = args[i]
                        assert(args[i])
                        const sourcecode = fileMap.get(expr.span.file)
                        assert(sourcecode)
                        const exprText = sourcecode.slice(expr.span.from, expr.span.to)
                        assert(exprText)
                        args[index] = bindExpression({ kind: 'string', value: exprText })
                    }
                } else if (param.notes.has('call site')) {
                    assert(param.notes.get('call site').length == 0)
                    if (!args[i]) {
                        // inject call site ;3
                        args[i] = bindExpression({ kind: 'string', value: callsite })
                    }

                } else {
                    console.log('unhandled notes?')
                    console.log(param.notes)
                    assert(false)
                }
            }

            assert(args[i], `argument supplied for each parameter`)

            let arg = args[i]

            assert(param.type, `param has a type`)
            assert(arg.type, `arument has a type`)

            // string literal -> char literal
            if (arg.kind == 'stringLiteral') {
                if (param.type.type == 'char' && arg.type.type == 'string') {
                    assert(arg.len == 1)
                    arg.type = typeMap.char
                }
            }

            // implicit cstring -> *char AND string -> *void cast
            // implicit string -> *char AND string -> *void cast
            if (param.type.type == 'pointer' && (arg.type.type == 'cstring' || arg.type.type == 'string')) {
                const toType = param.type.to.type
                if (toType == 'void' || toType == 'char') {
                    const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                    arg = cast
                    args[i] = arg
                }
            }

            // implicit []foo -> *foo cast AND []foo -> *void cast
            if (param.type.type == 'pointer' && arg.type.type == 'array') {
                if (param.type.to.type == arg.type.of.type || param.type.to.type == 'void') {
                    const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                    arg = cast
                    args[i] = arg
                }
            }

            // TODO: introduce explicit cast
            if (param.type.type == 'int' && arg.type.type == 'char') {
                const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                arg = cast
                args[i] = arg
            }

            // TODO: introduce explicit cast
            if (arg.type.type == 'void') {
                const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                arg = cast
                args[i] = arg
            }

            if (param.type.kind == 'union') {
                const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                arg = cast
                args[i] = arg
            }

            // if (param.type.type != arg.type.type) {
            //     console.log("expected:")
            //     console.log(param.type)
            //     console.log("got:")
            //     console.log(arg)
            // }
            assert(param.type.type == arg.type.type, 'parameter type matches argument type')
        }

        return args
    }

    function bindParameters(params) {
        if (!params) return []
        return params.items.filter((_, i) => i % 2 == 0).map(p => {
            const it = {
                kind: 'parameter',
                name: p.name.value,
                type: bindType(p.type),
                notes: new Map(),
            }

            it.span = spanFromRange(p.name.span, it.type.span)
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
            const it = { kind: 'scope parameter' }
            const name = p.name.value
            assert(name)
            const type = bindType(p.type)
            const symbol = findSymbol(name, currentScope().parent)
            assert(symbol)
            assert(type)
            assert(symbol.type)
            assert(type.type == symbol.type.type)
            it.symbol = symbol
            currentScope().symbols.set(name, symbol)
            it.span = spanFromRange(p.name.span, type.span)
            return it
        })
    }

    function bindList(list, binder) {
        if (!list) return []
        return list.filter((_, i) => i % 2 == 0).map(p => {
            return binder(p)
        })
    }

    function bindBlock(body, isExpression = false) {
        let boundStatements = new Array(body.statements.length)

        // first pass: declarations
        for (let i in body.statements) {
            const stmt = body.statements[i]
            if (stmt.kind == 'label') {
                console.log('label first')
                boundStatements[i] = bindDeclaration(stmt)
            }
        }

        // second pass: expressions
        for (let i in body.statements) {
            const stmt = body.statements[i]
            if (!boundStatements[i]) {

                if (stmt.kind == 'goto') console.log('goto :)')
                boundStatements[i] = bindDeclaration(stmt)
            }
        }

        return {
            kind: 'block',
            isExpression,
            statements: boundStatements,
            span: spanFromRange(body.begin.span, body.end.span)
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

    function mangleLabel(name) {
        const count = labelCount.get(name) ?? 0
        labelCount.set(name, count + 1)
        const newName = count == 0
            ? name
            : name + btoa(count).replace(/=/g, '')
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

    function lowerNodeList(nodes) {
        if (!nodes) return null
        const newList = []
        for (let n of nodes) {
            const result = lowerNode(n)

            if (result) {
                assert(Array.isArray(result))
                for (let r of result) {
                    assert(!Array.isArray(r))
                    newList.push(r)
                }
            }
        }
        return newList
    }

    function lowerNode(node, { makeBuffer = true } = {}) {
        switch (node.kind) {
            case 'import':
            case 'type alias':
            case 'use': return []

            case 'unary':
            case 'numberLiteral':
            case 'booleanLiteral':
            case 'charLiteral': return [node]

            case 'implicit cast': {

                if (node.expr.type.type == 'void') {
                    // HACK: just set the type on the expr and lower that
                    node.expr.type = node.type
                    return lowerNode(node.expr)
                }

                if (node.type.type == 'pointer' && (node.expr.type.type == 'string' || node.expr.type.type == 'array')) {
                    // TODO: turn this into .buffer property access
                    // currently handled as some hacky edge case in compiler.js
                    return lowerNode(node.expr)
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

            case 'stringLiteral': {

                if (makeBuffer) {
                    if (node.type.type != 'char') {
                        if (buffers.includes(node)) {
                            // this can legitimately happen if the string is referenced twice
                            // throw "bruh"
                        } else {
                            buffers.push(node)
                        }
                    }
                }

                return [node]
            }

            case 'arrayLiteral': {
                node.entries = lowerNodeList(node.entries)

                if (makeBuffer) {
                    if (buffers.includes(node)) {
                        throw "bruh"
                    } else {
                        buffers.push(node)
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

                return [node]
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
                let indexInitializer = num(0, cloneType(typeMap.int))
                if (node.index) node.index.expr = indexInitializer
                let i = node.index ?? declareVar('i', indexInitializer)
                let begin = label('begin')
                let endLabel = label('end')
                let condition = goto(endLabel, binary('>=', ref(i), readProp(ref(node.list), { kind: 'string length' })))
                let item = node.item
                let setItem = assignVar(ref(item), offsetAccess(ref(node.list), ref(i)))
                let body = node.block
                let inc = unary('post++', ref(i))
                let loop = goto(begin)

                let prevBreak = breakLabel
                let prevContinue = continueLabel
                breakLabel = endLabel
                continueLabel = begin
                const result = lowerNodeList([i, begin, item, condition, setItem, body, inc, loop, endLabel])
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

            case 'scope': return lowerNodeList(node.instructions.statements)
            case 'if': {
                const cond = lowerNode(node.cond)
                assert(cond.length && cond.length == 1)
                node.cond = cond[0]
                node.then = lowerNodeList(node.then.statements)
                if (node.els) {
                    if (node.els.kind == 'if') {
                        node.els = lowerNode(node.els)
                    } else {
                        assert(node.els.kind == 'block')
                        node.els = lowerNodeList(node.els.statements)
                    }
                }
                return [node]
            }

            case 'declareVar': {
                node.name = mangleName(node)
                let expr

                if (!node.expr && node.type.type == 'array' && node.type.count) {
                    assert(node.type.of.type == 'char')
                    node.expr = {
                        kind: 'arrayLiteral',
                        entries: new Array(node.type.count).fill(null).map(_ => num(0)),
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

                    // else {
                    //     return [node]
                    // }
                }

                if (node.expr) {
                    const varDec = node
                    // don't lower if we already did so above
                    if (!expr) {
                        expr = node.expr ? lowerNode(node.expr) : null
                    }
                    assert(!expr || expr.length == 1)
                    expr = expr[0]
                    const ref = { kind: 'reference', symbol: varDec, type: varDec.type }
                    const varAssignment = { kind: 'assignVar', varDec: ref, expr }
                    varDec.expr = undefined
                    return [varDec, varAssignment]
                } else {
                    return [node]
                }
            }
            case 'function': {
                // NOTE: we can't create a new copy because this would break the symbol
                node.name = mangleName(node)

                const isEntrypoint = node.notes.has('entrypoint')
                if (isEntrypoint) {
                    assert(!entrypoint)
                    entrypoint = node;
                }
                const instructions = lowerNodeList(node.instructions?.statements)
                if (instructions) {
                    if (buffers.length) {
                        buffers = buffers.map(b => ({ kind: 'buffer', data: b }))
                    }
                    node.instructions = [...buffers, ...instructions]
                }
                buffers = []
                return [
                    node
                ]
            }

            case 'union':
            case 'struct': {
                node.name = mangleName(node)
                return []
            }

            case 'block': return lowerNodeList(node.statements)

            case 'binary': {
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
                const right = lowerNode(node.b)
                assert(left.length == 1)
                assert(right.length == 1)
                node.a = left[0]
                node.b = right[0]

                // HACK: bad constant folding
                if (node.a.kind == 'numberLiteral' && node.b.kind == 'numberLiteral') {
                    // NOTE: tight coupling between x3 and js operators
                    const newValue = eval(`node.a.n ${node.op} node.b.n`)
                    const it = { ...node.a, n: Number(newValue), type: node.type }
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

                const expr = lowerNode(node.expr)

                assert(expr.length == 1)
                node.expr = expr[0]
                return [node]
            }
            case 'reference': {
                if (node.symbol.kind == 'declareVar') {
                    if (node.symbol.notes.has('const') && node.symbol.expr) {
                        return lowerNode(node.symbol.expr)
                    }
                }

                return [node]
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
            case 'ctorcall':
            case 'syscall':
            case 'call': {
                if (node.kind == 'call' && node.def.symbol.name == 'typeof') {
                    // const nodeToGetTypeOf = node.args[0]
                    // const type = nodeToGetTypeOf.type
                    // const typeinfo = node.type
                    // const typeAccess = unary('&', ref(node.info), node.type)

                    const typeAccess = ref(node.info)
                    // HACK: labels ALREADY act as pointers, so just make the type a pointer for now
                    // we should fix this eventually...
                    typeAccess.type = node.type

                    // assert(type)
                    // console.log(node)
                    // const typeAccess = ctor(typeinfo,/*kind*/ num(0),/*name*/str(type.type, typeMap.string))
                    // console.log(typeAccess)
                    return lowerNode(typeAccess)
                }
                return [{
                    ...node, args: node.args.map(a => {
                        const result = lowerNode(a)
                        assert(result.length == 1)
                        return result[0]
                    })
                }]
            }
            case 'offsetAccess': {
                const expr = lowerNode(node.index)
                assert(expr.length == 1)
                node.index = expr[0]
                return [node]
            }
            case 'readProp': {

                if (node.prop.kind == 'string length') {
                    return [node]
                }

                if (node.left.kind == 'reference') {
                    if (node.left.symbol.kind == 'module') {
                        return lowerNode(node.prop)
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
