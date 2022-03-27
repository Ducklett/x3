function parse(sources) {
    const code = sources.join('\n')
    console.log(code)

    const keywords = new Set(["module", "use", "struct", "union", "proc", "return", "break", "continue", "goto", "var", "const", "for", "do", "while", "each", "enum", "if", "else", "switch",])
    const operators = new Set(["<<=", ">>=", "&&=", "||=", "==", "!=", ">=", "<=", "<<", ">>", "->", "&&", "||", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=", "!", ">", "<", "+", "-", "/", "*", "%", "^", "~", "&", "|", "(", ")", "[", "]", "{", "}", "?", ":", ";", ".", ","])
    const binaryOperators = new Set(["==", "!=", ">=", "<=", "<<", ">>", "&&", "||", ">", "<", "+", "-", "/", "*", "%", "^", "&", "|"])
    const assignmentOperators = new Set(["<<=", ">>=", "&&=", "||=", "==", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=",])

    function lex(code) {
        function isWhitespace(c) {
            return c == ' ' || c == "\n" || c == "\r" || c == '\t'
        }

        function isLegalKeyword(c) {
            return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        }

        function isLegalSymbol(c) {
            return c == ' ' || isLegalKeyword(c) || (c >= '0' && c <= '9')
        }

        function isLegalNumber(c) {
            return (c >= 0 && c <= 9)
        }

        function isLegalHexNumber(c) {
            return (c >= 0 && c <= 9) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
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

            let from = lexerIndex
            while (isWhitespace(current())) lexerIndex++
            if (from != lexerIndex) {
                tokens.push({ kind: 'whitespace', value: code.slice(from, lexerIndex) })
            }

            if (code[lexerIndex] === undefined) break

            // comment
            if (current() == '(' && peek(1) == '(') {
                let from = lexerIndex + 2
                while (!(current() == ')' && peek(1) == ')')) lexerIndex++
                const comment = code.slice(from, lexerIndex)
                tokens.push({ kind: 'comment', value: comment })
                lexerIndex += 2
                continue
            }
            // tag
            if (current() == '#') {
                lexerIndex++
                let from = lexerIndex
                while (isLegalSymbol(current())) lexerIndex++
                if (lexerIndex == from) throw 'expected symbol after pound symbol'
                tokens.push({ kind: 'tag', value: code.slice(from, lexerIndex) })
                continue
            }

            //string
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
                lexerIndex++
                tokens.push({ kind: 'string', value: str })
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
                    tokens.push({ kind: 'number', value: num })
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
                    tokens.push({ kind: 'number', value: num })
                    continue
                }
            }

            // keyword and symbol
            if (isLegalKeyword(current())) {
                let from = lexerIndex
                while (isLegalKeyword(current())) lexerIndex++
                let potentialKeyword = code.slice(from, lexerIndex)
                if (keywords.has(potentialKeyword)) {
                    tokens.push({ kind: 'keyword', value: potentialKeyword })
                    continue
                } else {
                    while (isLegalSymbol(current())) lexerIndex++
                    // remove trailing whitespace
                    while (isWhitespace(peek(-1))) lexerIndex--

                    let symbol = code.slice(from, lexerIndex)
                    tokens.push({ kind: 'symbol', value: symbol })
                    continue
                }
            }

            // token
            {
                let from = lexerIndex
                let operatorLen = maxTokenLength
                while (operatorLen) {
                    const op = code.slice(from, from + operatorLen)
                    if (operators.has(op)) {
                        lexerIndex = from + operatorLen
                        tokens.push({ kind: 'operator', value: op })
                        continue lex;
                    }
                    operatorLen--
                }
            }

            throw `unexpected character '${code[lexerIndex]}'`
        }

        return tokens
    }

    function parse(tokens) {
        tokens = tokens.filter(t => t.kind != 'whitespace' && t.kind != 'comment')
        let tokenIndex = 0
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
                throw (`expected token of kind ${kind}, got ${got.kind}::${got.value}`)
            }

            if (value !== null && got.value != value) {
                throw (`expected token with value ${value}, got ${got.value}`)
            }
        }

        function take(kind, value) {
            expect(kind, value)
            return tokens[tokenIndex++];
        }

        function assert(expr, msg) {
            if (expr) return
            console.error(`assertion failed: ${msg}`)
            process.exit(1)
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
                lhs = { kind: 'binary', lhs, op, rhs }
            }
            return lhs

            function parsePrimaryExpression() {
                switch (current().kind) {
                    case 'number': return take('number')
                    case 'string': return take('string')

                    case 'symbol': {
                        let name = take('symbol')
                        if (is('operator', '(')) {
                            // function call
                            const argumentList = parseList(parseExpression, "()")
                            return { kind: 'call', name, argumentList }
                        } else if (is('operator', '.')) {
                            // property acces
                            const dot = take('operator', '.')
                            const property = take('symbol')

                            // use property access as lhs, try for assignement
                            name = { kind: 'property access', name, dot, property }
                        }

                        if (isAssignmentOperator(current())) {
                            const operator = take('operator')
                            const expr = parseExpression()
                            return { kind: 'assignment', name, operator, expr }
                        }
                        else {
                            return { kind: 'reference', name }
                        }

                        // from property access
                        return name
                    }
                    case 'keyword': {
                        switch (current().value) {
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

        function parseDeclaration() {
            const tags = []
            while (is('tag')) {
                const tag = take('tag')
                // TODO: maybe make this less gross
                let list
                if (is('operator', '(')) list = parseList(parseExpression, '()')
                tag.list = list
                tags.push(tag)
            }

            const cur = current().value
            switch (cur) {
                case 'module': {
                    const keyword = take('keyword', 'module')
                    const name = take('symbol')
                    const block = parseBlock('module', true, false)
                    return { kind: 'module', keyword, name, block, tags }
                }
                case 'proc': {
                    const keyword = take('keyword', 'proc')
                    const name = take('symbol')
                    const parameters = parseList(parseTypedSymbol)
                    let arrow, returnType
                    if (is('operator', '->')) {
                        arrow = take('operator', '->')
                        returnType = parseType()
                    }
                    let body
                    if (is('operator', ';')) {
                        body = take('operator', ';')
                    } else {
                        body = parseBlock('proc', true, true)
                    }
                    return { kind: 'proc', keyword, name, parameters, arrow, returnType, body, tags }
                }
                case 'struct': {
                    const keyword = take('keyword', 'struct')
                    const name = take('symbol')
                    const parameters = parseList(parseTypedSymbol)
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
                    terminator = take('operator', ';')
                    return { kind: 'var', keyword, name, colon, type, equals, expr, terminator, tags }
                }
                default:
                    if (tags.length) throw `tags with out declaration, got ${current().kind} ${current().value}`

                    return false
            }
        }

        function parseTypedSymbol() {
            const name = take('symbol')
            const colon = take('operator', ':')
            const type = parseType()
            return { kind: 'typed symbol', name, colon, type }
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
                    const needsTermination = new Set(['binary', 'unary', 'assignment', 'call', 'return'])
                    let expr = parseExpression()
                    if (expr) {
                        if (needsTermination.has(expr.kind)) {
                            expr = { kind: 'terminated expression', expr, terminator: take('operator', ';') }
                        }
                        statements.push(expr)
                        continue
                    }
                }
                throw 'failed to parse statement or expression in block'
            }
            const end = take('operator', '}')
            return { kind: 'block', begin, statements, end }
        }

        function parseType() {
            if (is('symbol')) {
                return { kind: 'type atom', name: take('symbol') }
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
            }
            throw `unhandled type ${current().kind}::${current().value}`

        }

        return parseFile()
    }

    const tokens = lex(code)
    const statements = parse(tokens)
    //emitHtml(statements)
    //console.log(JSON.stringify(statements, null, 2))
    return statements
}

// const code = `
// module demo {
// 	((this is a single line comment))
// 	var foo:int = 0;
// 	const PI:float = 3.1415;

// 	((
// 	proc stands for procedure
// 	we don't guarantee mathematical purity
// 	so calling them functions is a misnomer
// 	))
// 	proc square[n:int] -> int {
// 		return n * n;
// 	}

// 	struct vec2[x:float, y:float]

// 	((# is used for annotations, similar to hashtags in other domains))
// 	#entrypoint
// 	proc main[args:[]string] {
// 		((note how the syntax is designed so
// 		  we can have spaces in symbol names*))
// 		var my vector = vec2(10,20);
// 		((
// 		dot syntax is unnatural
// 		but > is already used by gt
// 		and -> is more typing..
// 		))
// 		my vector.x += 3;
// 		my vector.y *= 2;
// 		assert(my vector.x == 13);
// 		assert(my vector.y == 40);

// 		each arg : args { print(arg); }

// 		((TODO: more fun stuff here))
// 	}

// 	((
// 	note how functions use [] for declaring their parameters
// 	this makes more sense since they're a LIST of parameters
// 	since our declaration syntax is universal you can take:
// 	fn add[x:int,y:int]
// 	and turn it into:
// 	struct vec2i[x:int,y:int]
// 	just by changing the keyword and name you've refactored
// 	your function arguments into a separate data structure.
// 	))
// }
// `.trim()

module.exports = { parse }
