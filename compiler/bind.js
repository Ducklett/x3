const { assert } = require('./compiler')
const { fileMap } = require('./parser')

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

    // TODO: actually have different int types
    const typeMap = {
        'int': { type: 'int', size: 8 },
        'uint': { type: 'int', size: 8 },
        'u64': { type: 'int', size: 8 },
        'i64': { type: 'int', size: 8 },
        'u0': { type: 'u0', size: 0 },
        'string': { type: 'string', size: 16 },
        'cstring': { type: 'cstring', size: 8 },
        'char': { type: 'char', size: 1 },
        'bool': { type: 'bool', size: 1 },
        'pointer': { type: 'pointer', size: 8, to: undefined },
    }

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
                const isCharBuffer = node.type && node.type.kind == 'type array'
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
                if (it.expr) assert(it.expr.type == it.type, `type matches ${it.expr.type.type} ${it.type.type}`)

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

                assert(list.type.kind == 'type array')
                item.type = list.type.of

                const block = bindBlock(node.block)

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
                const type = typeMap[name]
                assert(type, `'${name}' is a legal type`)
                type.span = span
                return type
            }
            case 'type array': {
                // HACK: string buffer type; reinterpret as string
                assert(node.of.kind == 'type atom' && node.of.name.value == 'char')
                const type = typeMap.string
                type.span = spanFromRange(node.begin.span, node.of.name.span)
                return type
            }
            case 'type pointer': {
                const it = typeMap.pointer
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
                return { kind: 'numberLiteral', n: node.value, type: typeMap.int, span: node.span }
            case 'string':
                return {
                    kind: 'stringLiteral',
                    value: node.value,
                    len: node.value.length,
                    type: node.value.endsWith('\0')
                        ? typeMap.cstring
                        : typeMap.string,
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
                    kind: 'type array',
                    count: it.entries.length,
                    of: entryType,
                    size: 16,
                    span: compilerSpan()
                }

                it.type = arrayType

                for (let i = 0; i < it.entries.length; i++) {
                    assert(it.entries[i].type == entryType)
                }

                return it
            }
            case 'parenthesized expression': {
                const expr = bindExpression(node.expr);
                assert(expr.type)
                const type = expr.type
                return { kind: 'parenthesized expression', expr, type, span: spanFromRange(node.open.span, node.close.span) }
            }
            case 'symbol': {
                // string length hack
                if (node.value == 'length') {
                    return { kind: 'string length', type: typeMap.u64, span: node.span }
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
                console.log(node)
                const left = bindExpression(node.name)
                assert(left)
                assert(left.symbol.type.kind == 'type array', `can only access offset of arrays`)

                const index = bindExpression(node.index)
                assert(index.kind == 'numberLiteral')

                const it = {
                    kind: 'offsetAccess',
                    left,
                    index,
                    type: left.symbol.type.of,
                    span: spanFromRange(left, node.end.span)
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
                const type = isLogical ? typeMap.bool : a.type
                const it = { kind: 'binary', a, op, b, type, span: spanFromRange(a.span, b.span) }
                return it
            }
            case 'assignment': {
                const varDec = bindExpression(node.name)
                assert(varDec, `symbol "${node.name.value}" is defined`)

                assert(node.operator.value == '=')

                const expr = bindExpression(node.expr)
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

            if (param.type.type == 'pointer' && arg.type.type == 'string') {
                const toType = param.type.to.type
                if (toType == 'u0' || toType == 'char') {
                    const cast = { kind: 'implicit cast', type: param.type, expr: arg }
                    arg = cast
                    args[i] = arg
                }
            }

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
        return {
            kind: 'block',
            isExpression,
            statements: body.statements.map(bindDeclaration),
            span: spanFromRange(body.begin.span, body.end.span)
        }
    }
}

function lower(ast) {
    let entrypoint

    const loweredAst = lowerNodeList(ast)
    return [loweredAst, { entrypoint }]

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
            case 'stringLiteral': return [node]

            case 'arrayLiteral': {
                node.entries = lowerNodeList(node.entries)
                return [node]
            }
            case 'each': {
                node.block = lowerNode(node.block)
                return [node]
            }
            case 'return': {
                if (node.expr) {
                    const expr = lowerNode(node.expr)
                    assert(expr.length && expr.length == 1)
                    node.expr = expr[0]
                }
                return [node]
            }

            case 'parenthesized expression': return lowerNode(node.expr)

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
