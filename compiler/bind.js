const { assert } = require('./compiler')

function bind(files) {
    const bodies = new Map()
    const usings = []
    const ast = []
    const scopeStack = []
    const fileScopes = []

    const typeMap = {
        'int': { type: 'int', size: 8 },
        'string': { type: 'string', size: 16 },
    }

    pushScope(null, 'root', 'global')

    function currentScope() { return scopeStack[scopeStack.length - 1]; }

    for (let root of files) {
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
                    if (!it.type) it.type = it.expr.type
                }

                assert(it.type, 'type must be either provided or inferred from expression')
                if (it.expr) assert(it.expr.type == it.type, 'type matches')

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
                const it = {
                    kind: 'function',
                    name: node.name.value,
                    instructions: undefined,
                    notes: new Map()
                }
                for (let n of node.tags) {
                    it.notes.set(...bindTag(n))
                }
                addSymbol(it.name, it)
                it.scope = pushScope(null, it.name)
                it.params = bindParameters(node.parameters)
                popScope()

                if (node.body.kind == 'block') bodies.set(it, node)
                else it.instructions = null

                return it
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


    function bindType(node) {

        switch (node.kind) {
            case 'type atom': {
                assert(node.name.kind == 'symbol')
                const type = typeMap[node.name.value]
                assert(type, `'${node.name.value}' is a legal type`)
                return type
            }
            case 'type array': {
                // HACK: string buffer type; reinterpret as string
                assert(node.of.kind == 'type atom' && node.of.name.value == 'char')
                return typeMap.string
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
                return { kind: 'numberLiteral', n: node.value, type: typeMap.int }
            case 'string':
                return {
                    kind: 'stringLiteral',
                    value: node.value,
                    len: node.value.length,
                    type: typeMap.string
                }
            case 'symbol': {
                // string length hack
                if (node.value == 'length') {
                    return { kind: 'string length' }
                }

                const symbol = findSymbol(node.value, inScope)
                assert(symbol, `symbol "${node.value}" is defined`)
                const it = {
                    kind: 'reference',
                    symbol
                }
                return it
            }
            case 'property access': {
                const left = bindExpression(node.scope, inScope)
                assert(left)
                assert(left.symbol.scope || node.property.value == 'length', `property access is either string length hack OR module`)

                const right = bindExpression(node.property, left.symbol?.scope)

                const it = {
                    kind: 'readProp',
                    left,
                    prop: right
                }
                return it
            }
            case 'call': {
                const def = bindExpression(node.name, inScope)

                const isSyscall = def.symbol.notes.has('syscall')

                if (isSyscall) {
                    const it = {
                        kind: 'syscall',
                        code: def.symbol.notes.get('syscall')[0],
                        args: bindArguments(node.argumentList)
                    }
                    return it
                } else {
                    const it = {
                        kind: 'call',
                        def,
                        args: bindArguments(node.argumentList)
                    }
                    return it
                }
            }
            case 'binary': {
                const a = bindExpression(node.lhs)
                const b = bindExpression(node.rhs)
                const op = node.op.value
                const it = { kind: 'binary', a, op, b }
                return it
            }
            case 'assignment': {
                const varDec = bindExpression(node.name)
                assert(varDec, `symbol "${node.name.value}" is defined`)
                const expr = bindExpression(node.expr)
                const it = {
                    kind: 'assignVar',
                    varDec,
                    expr
                }
                return it
            }
            default:
                assert(false, `unhandled kind "${node.kind}"`)
        }
    }

    function bindParameters(params) {
        return params.items.filter((_, i) => i % 2 == 0).map(p => {
            const it = { kind: 'parameter', name: p.name.value }
            addSymbol(it.name, it)
            return it
        })
    }
    function bindScopeParameters(params) {
        return params.items.filter((_, i) => i % 2 == 0).map(p => {
            const it = { kind: 'scope parameter' }
            const name = p.name.value
            assert(name)
            const symbol = findSymbol(name, currentScope().parent)
            assert(symbol)
            it.symbol = symbol
            currentScope().symbols.set(name, symbol)
            return it
        })
    }
    function bindArguments(args) {
        return args.items.filter((_, i) => i % 2 == 0).map(p => {
            return bindExpression(p)
        })
    }
    function bindBlock(body, isExpression = false) {
        return { kind: 'block', isExpression, statements: body.statements.map(bindDeclaration) }
    }
}

function lower(ast) {
    return lowerNodeList(ast)

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
            case 'use': return []

            case 'module': return lowerNodeList(node.declarations.statements)

            case 'scope': return lowerNodeList(node.instructions.statements)

            case 'declareVar': {
                node.name = mangleName(node)
                if (node.notes.has('const')) {
                    return [node]
                }

                if (node.expr) {
                    const varDec = node
                    let expr = node.expr ? lowerNode(node.expr) : null
                    assert(!expr || expr.length == 1)
                    expr = expr[0]
                    const varAssignment = { kind: 'assignVar', varDec, expr }
                    varDec.expr = undefined
                    return [varDec, varAssignment]
                } else {
                    return [node]
                }
            }
            case 'function': {
                // NOTE: we can't create a new copy because this would break the symbol
                node.name = mangleName(node)
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
                return [node]
            }

            case 'assignVar': {
                const expr = lowerNode(node.expr)
                assert(expr.length == 1)
                node.expr = expr[0]
                return [node]
            }
            case 'reference':
            case 'numberLiteral':
            case 'stringLiteral': return [node]

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
            case 'readProp': {
                if (node.left.kind == 'reference' && node.left.symbol.kind == 'module') {
                    return lowerNode(node.prop)
                } else {
                    return [node]
                }
            }
            default:
                console.log(node)
                throw `lowering not implemented for ${node.kind}`
        }
    }
}

module.exports = { bind, lower }
