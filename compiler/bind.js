const { assert, syscall } = require('./compiler')

function bind(root) {
    assert(root.kind == 'file')

    // only globals for now
    const symbols = new Map()
    const ast = []

    bindFile(root)
    return ast

    function bindFile(node) {
        for (let decl of node.declarations) {
            const it = bindDeclaration(decl)
            assert(it)
            ast.push(it)
        }
    }

    function bindDeclaration(node) {
        switch (node.kind) {
            case 'var': {
                assert(
                    !symbols.has(node.name.value),
                    `${node.name.value} is a unique name`
                )
                const it = {
                    kind: 'declareVar',
                    name: node.name.value,
                    expr: undefined,
                    notes: new Map()
                }
                const isConst = node.keyword.value == 'const'
                if (isConst) it.notes.set('const', [])
                const isCharBuffer = node.type && node.type.kind == 'type array'
                if (isConst && isCharBuffer) {
                    console.log(node.type)
                    assert(node.type.size, `const array must have size`)
                    assert(node.type.of.kind == 'type atom', `must not be nested array`)
                    assert(node.type.of.name.value == 'char', `must be char`)
                    node.expr = { kind: 'string', value: "".padStart(node.type.size.value, ' ') }
                }
                symbols.set(it.name, it)

                if (node.expr) it.expr = bindExpression(node.expr)

                return it
            }
            case 'proc': {
                assert(
                    !symbols.has(node.name.value),
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
                symbols.set(it.name, it)
                it.params = bindParameters(node.parameters)
                if (node.body.kind == 'block') it.instructions = bindBody(node.body, it)
                else it.instructions = null

                return it
            }
            case 'terminated expression': {
                return bindExpression(node.expr)
            }
            default:
                assert(false, `unhandled kind "${node.kind}"`)
        }
    }

    function bindTag(node) {
        const t = node.value
        let args = []
        if (node.list)
            args = node.list.items.filter((_, i) => i % 2 == 0).map(bindExpression)
        return [t, args]
    }

    function bindExpression(node) {
        switch (node.kind) {
            case 'number':
                return { kind: 'numberLiteral', n: node.value }
            case 'string':
                return {
                    kind: 'stringLiteral',
                    value: node.value,
                    len: node.value.length
                }
            case 'reference': {
                if (node.name.kind == 'symbol') {
                    const varDec = symbols.get(node.name.value)
                    assert(varDec, `symbol "${node.name.value}" is defined`)
                    const it = {
                        kind: 'readVar',
                        varDec
                    }
                    return it
                } else if (node.name.kind == 'property access') {
                    const varDec = symbols.get(node.name.name.value)
                    assert(varDec, `symbol "${node.name.name.value}" is defined`)
                    const it = {
                        kind: 'readProp',
                        varDec,
                        prop: node.name.property.value
                    }
                    return it
                }
            }
            case 'call': {
                const def = symbols.get(node.name.value)
                assert(def, `symbol "${node.name.value}" is defined`)

                const isSyscall = def.notes.has('syscall')
                if (isSyscall) {
                    const it = {
                        kind: 'syscall',
                        code: def.notes.get('syscall')[0],
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
                const varDec = symbols.get(node.name.value)
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
        return params.items.filter((_, i) => i % 2 == 0).map(p => p.name.value)
    }
    function bindArguments(args) {
        return args.items.filter((_, i) => i % 2 == 0).map(p => bindExpression(p))
    }
    function bindBody(body) {
        return body.statements.map(bindDeclaration)
    }
}

function lower(ast) {
    return lowerNodeList(ast)

    function lowerNodeList(nodes) {
        if (!nodes) return null
        const newList = []
        for (let n of nodes) {
            const result = lowerNode(n)
            if (result) {
                for (let r of result) newList.push(r)
            }
        }
        return newList
    }

    function lowerNode(node) {
        switch (node.kind) {
            case 'declareVar': {
                if (node.notes.has('const')) {
                    return [node]
                }

                if (node.expr) {
                    const varDec = node
                    const varAssignment = { kind: 'assignVar', varDec, expr: node.expr }
                    varDec.expr = undefined
                    return [varDec, varAssignment]
                } else {
                    return node
                }
            }
            case 'syscall':
                return [node]
            case 'function': {
                return [
                    {
                        kind: 'function',
                        name: node.name,
                        instructions: lowerNodeList(node.instructions),
                        notes: node.notes,
                        params: node.params
                    }
                ]
            }
            default:
                console.log(node)
                throw `lowering not implemented for ${node.kind}`
        }
    }
}

module.exports = { bind, lower }
