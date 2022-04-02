const { read, assert } = require('./compiler')
const path = require("path")

const fileMap = new Map()

function parse(source) {

    fileMap.set(source.path, source.code)

    const code = source.code

    const keywords = new Set(["module", "import", "use", "type", "struct", "union", "proc", "scope", "return", "break", "continue", "goto", "var", "const", "for", "do", "while", "each", "enum", "if", "else", "switch", "true", "false"])
    const operators = new Set(["<<=", ">>=", "&&=", "||=", "==", "!=", ">=", "<=", "<<", ">>", "->", "&&", "||", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=", "!", ">", "<", "+", "-", "/", "*", "%", "^", "~", "&", "|", "(", ")", "[", "]", "{", "}", "?", ":", ";", ".", ","])
    const binaryOperators = new Set(["==", "!=", ">=", "<=", "<<", ">>", "&&", "||", ">", "<", "+", "-", "/", "*", "%", "^", "&", "|"])
    const assignmentOperators = new Set(["<<=", ">>=", "&&=", "||=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=",])

    const tokens = lex(code)
    const statements = _parse(tokens)
    //emitHtml(statements)
    //console.log(JSON.stringify(statements, null, 2))
    return statements

    function lex(code) {
        let span
        function startSpan() {
            span = { file: source.path, from: lexerIndex }
        }

        function takeSpan() {
            span.to = lexerIndex
            return span
        }

        function isWhitespace(c) {
            return c == ' ' || c == "\n" || c == "\r" || c == '\t'
        }

        function isLegalKeyword(c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        }

        function isLegalSymbol(c) {
            return c == ' ' || c == '_' || isLegalKeyword(c) || (c >= '0' && c <= '9')
        }

        function isLegalNumber(c) {
            return (c >= 0 && c <= 9)
        }

        function isLegalHexNumber(c) {
            return (c >= 0 && c <= 9) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        }

        function isLegalOctalNumber(c) {
            return (c >= 0 && c <= 7)
        }

        const maxTokenLength = 3;
        let lexerIndex = 0;
        const len = code.length

        let tokens = []

        lex:
        while (lexerIndex < len) {
            const current = () => {
                // overstepped by 2, should definitely just kys now to prevent inifinite loop ;)
                if (lexerIndex > len) throw "out of range!"
                return code[lexerIndex]
            }

            const peek = (n) => {
                return code[lexerIndex + n]
            }

            startSpan()
            let from = lexerIndex
            while (isWhitespace(current())) lexerIndex++
            if (from != lexerIndex) {
                tokens.push({ kind: 'whitespace', value: code.slice(from, lexerIndex), span: takeSpan() })
            }

            if (code[lexerIndex] === undefined) break

            startSpan()

            // comment
            if (current() == '(' && peek(1) == '(') {
                let from = lexerIndex + 2
                while (!(current() == ')' && peek(1) == ')')) lexerIndex++
                const comment = code.slice(from, lexerIndex)
                lexerIndex += 2
                tokens.push({ kind: 'comment', value: comment, span: takeSpan() })
                continue
            }

            // tag
            if (current() == '#') {
                lexerIndex++
                let from = lexerIndex
                // TODO: unify this with the symbol parser below
                if (current() == "'") lexerIndex++
                while (isLegalSymbol(current())) lexerIndex++
                while (isWhitespace(peek(-1))) lexerIndex--
                const value = code.slice(from, lexerIndex)
                if (current() == "'") lexerIndex++
                if (lexerIndex == from) throw 'expected symbol after pound symbol'
                tokens.push({ kind: 'tag', value, span: takeSpan() })
                continue
            }

            //string
            let shouldNullTerminate = false
            if (current() == 'c' && peek(1) == '"') {
                shouldNullTerminate = true
                lexerIndex++
            }

            if (current() == '"') {
                lexerIndex++
                let from = lexerIndex
                while (current() != '"') lexerIndex++
                let str = code.slice(from, lexerIndex)
                // TODO: maybe move this elsewhere
                str = str.replace(/\\n/g, '\n')
                    .replace(/\\r/g, '\r')
                    .replace(/\\t/g, '\t')
                    .replace(/\\0/g, '\0')
                if (shouldNullTerminate) str += '\0'
                lexerIndex++
                tokens.push({ kind: 'string', value: str, span: takeSpan() })
                continue
            }

            // number
            if (isLegalNumber(current())) {
                let from = lexerIndex
                if (current() == '0' && peek(1) == 'x') {
                    // hex
                    lexerIndex += 2
                    let from = lexerIndex
                    while (isLegalHexNumber(current())) lexerIndex++

                    const slice = code.slice(from, lexerIndex)
                    let num = parseInt(slice, 16)
                    tokens.push({ kind: 'number', value: num, span: takeSpan() })
                    continue
                }
                else if (current() == '0' && peek(1) == 'o') {
                    // octal
                    lexerIndex += 2
                    let from = lexerIndex
                    while (isLegalOctalNumber(current())) lexerIndex++

                    const slice = code.slice(from, lexerIndex)
                    let num = parseInt(slice, 8)
                    tokens.push({ kind: 'number', value: num, span: takeSpan() })
                    continue

                } else {
                    // decimal
                    while (isLegalNumber(current())) lexerIndex++
                    // float
                    if (current() == '.') {
                        lexerIndex++
                        while (isLegalNumber(current())) lexerIndex++
                    }
                    let num = parseFloat(code.slice(from, lexerIndex))
                    tokens.push({ kind: 'number', value: num, span: takeSpan() })
                    continue
                }
            }

            // symbols may start with ' to prevent collisions with keywords
            var mightBeKeyword = true
            if (current() == "'" && isLegalSymbol(peek(1))) {
                lexerIndex++
                mightBeKeyword = false
            }

            // keyword and symbol
            if (isLegalKeyword(current()) || isLegalSymbol(current())) {
                let from = lexerIndex

                if (mightBeKeyword) {
                    while (isLegalKeyword(current())) lexerIndex++
                    let potentialKeyword = code.slice(from, lexerIndex)
                    if (keywords.has(potentialKeyword)) {
                        tokens.push({ kind: 'keyword', value: potentialKeyword, span: takeSpan() })
                        continue
                    }

                }

                while (isLegalSymbol(current())) lexerIndex++
                // remove trailing whitespace
                while (isWhitespace(peek(-1))) lexerIndex--

                let symbol = code.slice(from, lexerIndex)

                // symbols may end with ' to prevent collisions with keywords
                if (current() == "'") lexerIndex++

                tokens.push({ kind: 'symbol', value: symbol, span: takeSpan() })
                continue
            }

            // token
            {
                let from = lexerIndex
                let operatorLen = maxTokenLength
                while (operatorLen) {
                    const op = code.slice(from, from + operatorLen)
                    if (operators.has(op)) {
                        lexerIndex = from + operatorLen
                        tokens.push({ kind: 'operator', value: op, span: takeSpan() })
                        continue lex;
                    }
                    operatorLen--
                }
            }

            throw `unexpected character '${code[lexerIndex]}'`
        }

        return tokens
    }

    function _parse(tokens) {
        let isTopLevel = true
        const filesToImport = new Set()
        tokens = tokens.filter(t => t.kind != 'whitespace' && t.kind != 'comment')
        let tokenIndex = 0

        let contents = parseFile()
        const files = [contents]
        for (let filePath of filesToImport) {
            assert(filePath.value.startsWith('./'), `import file path always starts with ./`)
            let p = filePath.value.slice(2)
            if (!p.endsWith('.x3')) p += '.x3'

            const sourcePath = path.join(path.parse(source.path).dir, p)
            if (fileMap.has(sourcePath)) {
                console.log('NOTE: file already imported! skipping')
                continue
            }
            const importedSource = { path: sourcePath, code: read(sourcePath) }
            const parsedSource = parse(importedSource, fileMap)
            for (const file of parsedSource) {
                files.push(file)
            }
        }
        return files

        function peek(n) { return tokens[tokenIndex + n] }
        function current() { return peek(0) }
        function is(kind, value = null) {
            const got = current()
            return got.kind == kind && (value === null || got.value === value)
        }
        function currentIsBinaryOperator() {
            const cur = current()
            return cur.kind == 'operator' && binaryOperators.has(cur.value)
        }
        function isAssignmentOperator() {
            const cur = current()
            return cur.kind == 'operator' && assignmentOperators.has(cur.value)
        }
        function expect(kind, value = null) {
            const got = current()
            if (got.kind != kind) {
                const at = `${source.path}:${got.span.from}`
                throw (`${at}: expected token of kind ${kind}, got ${got.kind}::${got.value}`)
            }

            if (value !== null && got.value != value) {
                const at = `${source.path}:${got.span.from}`
                throw (`${at}: expected token with value ${value}, got ${got.value}`)
            }
        }

        function take(kind, value) {
            expect(kind, value)
            return tokens[tokenIndex++];
        }

        function parseFile() {
            const declarations = []
            while (true) {
                if (!current()) break
                const stmt = parseDeclaration('file')
                if (!stmt) assert(false, 'expected statement')
                declarations.push(stmt)
            }

            assert(!current(), "all tokens consumed at end of file")
            return { kind: 'file', declarations }
        }

        function parseExpression() {
            let lhs = parsePrimaryExpression()
            while (currentIsBinaryOperator()) {
                const op = take('operator')
                const rhs = parsePrimaryExpression()
                assert(rhs)
                lhs = { kind: 'binary', lhs, op, rhs }
            }
            return lhs

            function parsePrimaryExpression() {
                switch (current().kind) {
                    case 'operator': {
                        const v = current().value
                        if (v == '{') return parseBlock('expression', true, true)
                        if (v == ';') return null
                        if (v == '}') return null
                        if (is('operator', '(')) {
                            const open = take('operator', '(')
                            const expr = parseExpression()
                            const close = take('operator', ')')
                            return { kind: 'parenthesized expression', open, expr, close }
                        }
                        console.log(current())
                        assert(false, `unexpected operator`)
                    }
                    case 'number': return take('number')
                    case 'string': return take('string')

                    case 'symbol': {
                        let name = take('symbol')

                        if (is('operator', '.')) {
                            // property acces
                            const dot = take('operator', '.')
                            const property = parsePrimaryExpression()

                            // use property access as lhs, try for assignement
                            return { kind: 'property access', scope: name, dot, property }
                        }

                        if (is('operator', '(')) {
                            // function call
                            const argumentList = parseList(parseExpression, "()")
                            return { kind: 'call', name, argumentList }
                        }

                        if (isAssignmentOperator(current())) {
                            const operator = take('operator')
                            const expr = parseExpression()
                            return { kind: 'assignment', name, operator, expr }
                        }

                        // just a plain symbol
                        return name
                    }
                    case 'keyword': {
                        switch (current().value) {
                            case 'true': return { kind: 'boolean literal', value: true }
                            case 'false': return { kind: 'boolean literal', value: false }
                            case 'return': {
                                const keyword = take('keyword', 'return')
                                const expr = parseExpression()
                                return { kind: 'return', keyword, expr }
                            }
                            case 'each': {
                                const keyword = take('keyword', 'each')
                                const item = take('symbol')
                                const colon = take('operator', ':')
                                const list = take('symbol')
                                const block = parseBlock('each', true, true)

                                return { kind: 'each', keyword, item, colon, list, block }
                            }
                            default: throw `unexpected token ${current().kind}::${current().value} for expression`
                        }
                    }
                    default: throw `unexpected token ${current().kind}::${current().value} for expression`
                }
            }
        }

        function parseTags() {
            const tags = []
            while (is('tag')) {
                const tag = take('tag')
                // TODO: maybe make this less gross
                let list
                if (is('operator', '(')) list = parseList(parseExpression, '()')
                tag.list = list
                tags.push(tag)
            }
            return tags
        }

        function parseDeclaration() {
            const cur = current().value
            if (cur != 'import') isTopLevel = false

            switch (cur) {
                case 'import': {
                    assert(isTopLevel, `imports should only be at top level`)
                    const keyword = take('keyword', 'import')
                    const path = take('string')
                    filesToImport.add(path)
                    return { kind: 'import', keyword, path }
                }
                case 'use': {
                    const keyword = take('keyword', 'use')
                    const path = parsePath()
                    const it = { kind: 'use', keyword, path }
                    return it
                }
                case 'module': {
                    const keyword = take('keyword', 'module')
                    const name = take('symbol')
                    const block = parseBlock('module', true, false)
                    return { kind: 'module', keyword, name, block }
                }
                case 'type': {
                    const keyword = take('keyword', 'type')
                    const name = take('symbol')
                    const equals = take('operator', '=')
                    const type = parseType()
                    return { kind: 'type alias', keyword, name, equals, type }
                }
                case 'proc': {
                    const keyword = take('keyword', 'proc')
                    const name = take('symbol')
                    let parameters
                    if (is('operator', '[')) {
                        parameters = parseList(parseTypedSymbol)
                    }
                    let arrow, returnType
                    if (is('operator', '->')) {
                        arrow = take('operator', '->')
                        returnType = parseType()
                    }
                    const tags = parseTags()
                    let body
                    if (is('operator', '{')) {
                        body = parseBlock('proc', true, true)
                    } else if (is('operator', ';')) {
                        body = take('operator', ';')
                    }
                    return { kind: 'proc', keyword, name, parameters, arrow, returnType, body, tags }
                }
                case 'scope': {
                    const keyword = take('keyword', 'scope')
                    const name = take('symbol')
                    let parameters
                    if (is('operator', '[')) {
                        parameters = parseList(parseTypedSymbol)
                    }
                    const tags = parseTags()
                    let body = parseBlock('proc', true, true)
                    return { kind: 'scope', keyword, name, parameters, body, tags }
                }
                case 'struct': {
                    const keyword = take('keyword', 'struct')
                    const name = take('symbol')
                    const parameters = parseList(parseTypedSymbol)
                    const tags = parseTags()
                    return { kind: 'struct', keyword, name, parameters, tags }
                }
                case 'const':
                case 'var': {
                    const keyword = take('keyword')
                    const name = take('symbol')
                    let colon, type
                    if (is('operator', ':')) {
                        colon = take('operator')
                        type = parseType()
                    }
                    let equals, expr, terminator
                    if (is('operator', '=')) {
                        equals = take('operator', '=')
                        expr = parseExpression()
                    }
                    const tags = parseTags()
                    if (is('operator', ';')) terminator = take('operator', ';')
                    return { kind: 'var', keyword, name, colon, type, equals, expr, terminator, tags }
                }
                case 'if': {
                    const keyword = take('keyword', 'if')
                    const condition = parseExpression()
                    // TODO: don't allow 'real' declarations in if statement block, just control flow stuff
                    const thenBlock = parseBlock('if', true, true)
                    let elseKeyword, elseBlock
                    if (is('keyword', 'else')) {
                        elseKeyword = take('keyword', 'else')
                        elseBlock = parseBlock('if', true, true)
                    }

                    return { kind: 'if', keyword, condition, thenBlock, elseKeyword, elseBlock }
                }
                case 'return': {
                    const keyword = take('keyword', 'return')
                    let expr, terminator

                    if (is('operator', ';')) {
                        if (is('operator', ';')) terminator = take('operator', ';')
                    } else {
                        expr = parseExpression()
                        if (is('operator', ';')) terminator = take('operator', ';')
                    }

                    return { kind: 'return', keyword, expr, terminator }
                }
                default:
                    return false
            }
        }

        function parsePath() {
            return take('symbol')
        }

        function parseTypedSymbol() {
            const name = take('symbol')
            const colon = take('operator', ':')
            const type = parseType()
            const tags = parseTags()

            return { kind: 'typed symbol', name, colon, type, tags }
        }

        function parseList(itemParser, bookend = '[]') {
            assert(bookend && bookend.length == 2, 'the list bookend consists of an opening an closing character')

            const items = []
            const begin = take('operator', bookend[0])
            while (!is('operator', bookend[1])) {
                const item = itemParser()
                if (!item) throw 'failed to parse list item'
                items.push(item)
                if (!is('operator', bookend[1])) {
                    items.push(take('operator', ','))
                }
            }
            const end = take('operator', bookend[1])
            return { kind: 'list', begin, items, end }
        }

        function parseBlock(scope, allowDeclarations, allowExpressions) {
            const statements = []
            const begin = take('operator', '{')
            while (!is('operator', '}')) {
                if (allowDeclarations) {
                    const stmt = parseDeclaration(scope)
                    if (stmt) {
                        statements.push(stmt)
                        continue
                    }
                }
                if (allowExpressions) {
                    // TODO: looks like i'm handling return twice; fix this?
                    const allowsTermination = new Set(['binary', 'unary', 'assignment', 'call', 'return', 'property access'])
                    let expr = parseExpression()
                    if (expr) {
                        if (is('operator', ';') && allowsTermination.has(expr.kind)) {
                            expr = { kind: 'terminated expression', expr, terminator: take('operator', ';') }
                        }

                        statements.push(expr)
                        continue
                    }
                }

                console.log(current())
                throw 'failed to parse statement or expression in block'
            }
            const end = take('operator', '}')
            return { kind: 'block', begin, statements, end }
        }

        function parseChain() {
            let lhs = take('symbol')
            while (is('operator', '.')) {
                const dot = take('operator', '.')
                const rhs = take('symbol')
                lhs = { kind: 'property access', scope: lhs, dot, property: rhs }
            }

            return lhs
        }
        function parseType() {
            if (is('symbol')) {
                return { kind: 'type atom', name: parseChain() }
            } else if (is('operator', '[')) {
                let size
                let begin = take('operator', '[')
                if (is('number')) size = take('number')
                let end = take('operator', ']')
                let of = parseType()
                return { kind: 'type array', begin, size, end, of }
            } else if (is('operator', '*')) {
                let pointer = take('operator', '*')
                let to = parseType()
                return { kind: 'type pointer', pointer, to }
            } else if (is('operator', '!')) {
                let mutable = take('operator', '!')
                let it = parseType()
                return { kind: 'type mutable', mutable, it }
            }
            throw `unhandled type ${current().kind}::${current().value}`

        }
    }
}

module.exports = { parse, fileMap }
