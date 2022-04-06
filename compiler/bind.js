const { assert, declareVar, num, binary, ref, readProp, unary, label, goto, assignVar, offsetAccess, nop } = require('./compiler')
const { fileMap } = require('./parser')

// TODO: actually have different int types
const typeMap = {
    'int': { type: 'int', size: 8 },
    'uint': { type: 'int', size: 8 },
    'u64': { type: 'int', size: 8 },
    'i64': { type: 'int', size: 8 },
    'u0': { type: 'u0', size: 0 },
    'string': { type: 'string', size: 16 },  // *char, length
    'cstring': { type: 'cstring', size: 8 }, // *char
    'array': { type: 'array', size: 16 },    // *values,length
    'char': { type: 'char', size: 1 },
    'bool': { type: 'bool', size: 1 },
    'pointer': { type: 'pointer', size: 8, to: undefined },
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


    pushScope(null, 'root', 'global')

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

                // character buffer hack
                // TODO: implement real buffer type
                const isCharBuffer = node.type && node.type.type == 'array'
                if (isConst && isCharBuffer) {
                    assert(node.type.size, `const array must have size`)
                    assert(node.type.of.kind == 'type atom', `must not be nested array`)
                    assert(node.type.of.name.value == 'char', `must be char`)
                    node.expr = {
                        kind: 'string',
                        value: "".padStart(node.type.size.value, ' '),
                    }
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
                    : typeMap.u0

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
                    then: bindBlock(node.thenBlock),
                    els: node.elseBlock ? bindBlock(node.elseBlock) : null,
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
                const type = cloneType(typeMap[name])
                assert(type, `'${name}' is a legal type`)
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
                    assert(node.size.kind == 'number literal')
                    type.count = node.size.n
                }
                type.span = spanFromRange(node.begin.span, node.of.name.span)
                return type
            }
            case 'type pointer': {
                const it = cloneType(typeMap.pointer)
                it.to = bindType(node.to)
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
                if (it.op == '&') {
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
                assert(left.symbol.type.type == 'array' || left.symbol.type.type == 'string', `can only access offset of arrays or strings`)

                const index = bindExpression(node.index)

                const it = {
                    kind: 'offsetAccess',
                    left,
                    index,
                    span: spanFromRange(left, node.end.span)
                }
                if (left.symbol.type.type == 'array') {
                    it.type = left.symbol.type.of
                } else {
                    assert(left.symbol.type.type == 'string')
                    it.type = typeMap.char
                }
                return it
            }
            case 'property access': {
                const left = bindExpression(node.scope, inScope)
                assert(left)
                assert(left.symbol.scope || node.property.value == 'length', `property access is either string length hack OR module`)

                const right = bindExpression(node.property, left.symbol?.scope)
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

                assert(def.symbol.returnType)
                const type = def.symbol.returnType

                const isSyscall = def.symbol.notes.has('syscall')

                const params = def.symbol.params;

                if (isSyscall) {
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
                    const it = {
                        kind: 'call',
                        def,
                        args: bindList(node.argumentList.items, bindExpression),
                        type,
                        span: spanFromRange(node.name.span, node.argumentList.end.span)
                    }
                    it.args = validateArguments(it.args, params, node.name.span.file)

                    return it
                }
            }
            case 'binary': {
                const a = bindExpression(node.lhs)
                const b = bindExpression(node.rhs)
                assert(a.type)
                assert(b.type)
                assert(a.type.type == b.type.type)

                const op = node.op.value
                const logicalOperators = new Set(['>', '>=', '<', '<=', '==', '!='])
                const isLogical = logicalOperators.has(op)
                const type = isLogical ? cloneType(typeMap.bool) : a.type
                const it = { kind: 'binary', a, op, b, type, span: spanFromRange(a.span, b.span) }
                return it
            }
            case 'assignment': {
                const varDec = bindExpression(node.name)
                assert(varDec, `symbol "${node.name.value}" is defined`)

                assert(node.operator.value == '=')

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

            assert(param.type, `arument has a type`)
            assert(arg.type, `arument has a type`)

            // implicit string -> *char AND string -> *u0 cast
            if (param.type.type == 'pointer' && arg.type.type == 'string') {
                const toType = param.type.to.type
                if (toType == 'u0' || toType == 'char') {
                    const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                    arg = cast
                    args[i] = arg
                }
            }

            // implicit []foo -> *foo cast
            if (param.type.type == 'pointer' && arg.type.type == 'array') {
                if (param.type.to.type == arg.type.of.type) {
                    const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                    arg = cast
                    args[i] = arg
                }
            }

            // console.log(param.type)
            // console.log(arg.type)
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
            assert(type == symbol.type)
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

    function lowerNode(node) {
        switch (node.kind) {
            case 'import':
            case 'type alias':
            case 'use': return []

            // TODO: make implicit cast do something
            case 'implicit cast': return lowerNode(node.expr)

            case 'numberLiteral':
            case 'charLiteral':
            case 'unary':
            case 'stringLiteral': return [node]

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

            case 'arrayLiteral': {
                node.entries = lowerNodeList(node.entries)
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
                if (node.op == '++') node.op = 'pre++'
                else if (node.op == '--') node.op = 'pre--'

                node.kind = 'unary'

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

                return lowerNodeList([jumpToCondition, beginLabel, body, conditionLabel, jumpToBegin])
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

                return lowerNodeList([precondition, jumpToCondition, beginLabel, body, postCondition, conditionLabel, jumpToBegin])
            }
            case 'each': {
                let i = declareVar('i', num(0, cloneType(typeMap.int)))
                let begin = label('begin')
                let end = label('end')
                let condition = goto(end, binary('>=', ref(i), readProp(ref(node.list), { kind: 'string length' })))
                let item = node.item
                let setItem = assignVar(ref(item), offsetAccess(ref(node.list), ref(i)))
                let body = node.block
                let inc = unary('post++', ref(i))
                let loop = goto(begin)
                return lowerNodeList([i, begin, item, condition, setItem, body, inc, loop, end])
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
                if (node.els) node.els = lowerNodeList(node.els.statements)
                return [node]
            }

            case 'declareVar': {
                node.name = mangleName(node)
                if (node.notes.has('const')) {
                    const expr = lowerNode(node.expr)
                    assert(expr.length == 1)
                    node.expr = expr[0]

                    // inlined
                    // TODO: still add some comment to nasm so we know what the number represents
                    if (node.expr.kind == 'numberLiteral') {
                        return []
                    } else {
                        return [node]
                    }
                }

                if (node.expr) {
                    const varDec = node
                    let expr = node.expr ? lowerNode(node.expr) : null
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
                node.instructions = lowerNodeList(node.instructions?.statements)
                return [
                    node
                ]
            }

            case 'block': return lowerNodeList(node.statements)

            case 'binary': {
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
                const expr = lowerNode(node.expr)
                assert(expr.length == 1)
                node.expr = expr[0]
                return [node]
            }
            case 'reference': {
                if (node.symbol.kind == 'declareVar') {
                    if (node.symbol.notes.has('const')) {
                        return lowerNode(node.symbol.expr)
                    }
                }

                return [node]
            }

            case 'syscall':
            case 'call': {
                return [{
                    ...node, args: node.args.map(a => {
                        const result = lowerNode(a)
                        assert(result.length == 1)
                        return result[0]
                    })
                }]
            }
            case 'offsetAccess': {
                return [node]
            }
            case 'readProp': {
                if (node.left.kind == 'reference' && node.left.symbol.kind == 'module') {
                    return lowerNode(node.prop)
                } else {
                    return [node]
                }
            }
            default:
                console.log('??')
                console.log(node)
                throw `lowering not implemented for ${node.kind}`
        }
    }
}

module.exports = { bind, lower }
