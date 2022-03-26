module.exports = {
    format(syntaxtree, mark) {
        let pseudoOperators = new Set("[](){},.:;".split(''))
        let indentLevel = 0
        function space(n = 1) {
            return ''.padStart(n, ' ')
        }
        function indent() {
            indentLevel++
            return ''
        }
        function unindent() {
            indentLevel--
            return ''
        }
        function newline() {
            return '\n'
        }
        function indentation() {
            return ''.padStart(indentLevel, '\t')
        }
        function joinedByNewline(items) {
            return items.join('\n' + indentation())
        }
        function eachOnNewLine(items) {
            return items.map(v => indentation() + v).join('\n')
        }
        function separatedBySpace(items) {
            return items.join(' ')
        }
        function join(items) {
            return items.join('')
        }
        function formatTags(tags, onNewLine = true) {
            if (tags.length === 0) return ''
            return joinedByNewline(tags.map(_format)) + (onNewLine ? newline() + indentation() : space())
        }
        function _format(node) {
            switch (node.kind) {
                case 'file': return eachOnNewLine(node.declarations.map(_format))
                case 'tag': return join([
                    mark('comment', `#${node.value}`),
                    node.list ? _format(node.list) : ''
                ])
                case 'module': {
                    const str = []

                    formatTags(str, node.tags)

                    str.push(_format(node.keyword))
                    str.push(mark('symbol-const', _format(node.name)))
                    str.push(_format(node.block))
                    return separatedBySpace(str)
                }
                case 'proc': {
                    const hasBody = node.body.kind == 'block'
                    return join([
                        formatTags(node.tags, hasBody),
                        _format(node.keyword),
                        space(),
                        mark('symbol-code', _format(node.name)),
                        _format(node.parameters),
                        node.arrow ? [
                            space(),
                            _format(node.arrow),
                            space(),
                            _format(node.returnType),
                        ].join('') : '',
                        space(hasBody),
                        _format(node.body)
                    ])
                }
                case 'struct': {
                    return join([
                        formatTags(node.tags),
                        _format(node.keyword),
                        space(),
                        mark('symbol-code', _format(node.name)),
                        _format(node.parameters),
                    ])
                }
                case 'block': {
                    const begin = _format(node.begin)
                    indent()
                    const statements = eachOnNewLine(node.statements.map(_format))
                    unindent()
                    const end = indentation() + _format(node.end)
                    return join([begin, newline(), statements, newline(), end])
                }
                case 'list': {
                    const str = []
                    str.push(_format(node.begin))
                    let i = 0;
                    for (let item of node.items) {
                        let isSep = i % 2;
                        if (isSep && i != node.items.length - 1) {
                            str.push(_format(item) + space())
                        } else {
                            str.push(_format(item))
                        }
                        i++
                    }
                    str.push(_format(node.end))
                    return join(str)
                }
                case 'var': {
                    return join([
                        formatTags(node.tags),
                        _format(node.keyword),
                        space(),
                        mark(node.keyword.value == 'const' ? 'symbol-const' : 'symbol', _format(node.name)),
                        (node.colon) ? [
                            _format(node.colon),
                            _format(node.type),
                        ].join('') : '',
                        node.expr ? join([
                            space(),
                            _format(node.equals),
                            _format(node.expr),
                            _format(node.terminator),
                        ]) : '',
                    ])
                }
                case 'each': {
                    const str = []

                    str.push(_format(node.keyword))
                    str.push(mark('symbol', _format(node.item)))
                    str.push(_format(node.colon))
                    str.push(mark('symbol', _format(node.list)))
                    str.push(_format(node.block))
                    return separatedBySpace(str)
                }
                case 'binary': {
                    const str = []
                    str.push(_format(node.lhs))
                    str.push(_format(node.op))
                    str.push(_format(node.rhs))
                    return separatedBySpace(str)
                }
                case 'reference': {
                    return mark('symbol', _format(node.name))
                }
                case 'return': {
                    const str = []
                    str.push(_format(node.keyword))
                    str.push(_format(node.expr))
                    return separatedBySpace(str)
                }
                case 'call': {
                    const str = []
                    str.push(mark('symbol-code', _format(node.name)))
                    str.push(_format(node.argumentList))
                    return join(str)
                }
                case 'assignment': {
                    const str = []
                    str.push(mark('symbol', _format(node.name)))
                    str.push(_format(node.operator))
                    str.push(_format(node.expr))
                    return separatedBySpace(str)
                }
                case 'property access': {
                    const str = []
                    str.push(mark('symbol-const', _format(node.name)))
                    str.push(_format(node.dot))
                    str.push(mark('symbol', _format(node.property)))
                    return join(str)
                }
                case 'terminated expression': {
                    const str = []
                    str.push(_format(node.expr))
                    str.push(_format(node.terminator))
                    return join(str)
                }
                case 'typed symbol': {
                    const str = []

                    str.push(mark('symbol', _format(node.name)))
                    str.push(_format(node.colon))
                    str.push(_format(node.type))
                    return join(str)
                }
                case 'keyword': return mark('keyword', node.value)
                // its color will change depending on context, the parent will color it
                case 'symbol': return node.value
                case 'operator': return mark(pseudoOperators.has(node.value) ? 'pseudo-operator' : 'operator', node.value)
                case 'number': return mark('number', node.value)
                case 'string': return mark('string', `"${node.value}"`)

                // ========== types ==========
                case 'type atom': return mark('symbol-const', _format(node.name))
                case 'type pointer': return join([
                    mark('pseudo-operator', node.pointer.value),
                    _format(node.to)
                ])
                case 'type array': {
                    return join([
                        _format(node.begin),
                        _format(node.end),
                        _format(node.of)
                    ])
                }

                default: throw `unhandled node kind in formatter ${JSON.stringify(node)}`
            }
        }

        return _format(syntaxtree)
    }
}
