const keywords = new Set(["module", "import", "use", "type", "struct", "union", "proc", "return", "break", "continue", "goto", "label", "var", "const", "for", "do", "while", "each", "enum", "if", "else", "match", "true", "false", "null"])
const operators = new Set(["...", "<<=", ">>=", "&&=", "||=", "==", "!=", ">=", "<=", "<<", ">>", "<~", "~>", "<-", "->", "=>", "&&", "||", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=", "!", ">", "<", "+", "-", "/", "*", "%", "^", "~", "&", "|", "(", ")", "[", "]", "{", "}", "?", ":", ";", ".", ","])
const binaryOperators = new Set(["==", "!=", ">=", "<=", "<<", ">>", "&&", "||", "=>", ">", "<", "+", "-", "/", "*", "%", "^", "&", "|"])
const preUnaryOperators = new Set(["++", "--", "!", "-", "~>", "<~"])
const postUnaryOperators = new Set(["++", "--"])
const assignmentOperators = new Set(["<<=", ">>=", "&&=", "||=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "~=", "=",])

function lex(code, sourcePath = '<compiler>') {
	let span

	function startSpan() {
		span = { file: sourcePath, from: lexerIndex, fromLine: line, fromColumn: column }
	}

	function takeSpan() {
		span.to = lexerIndex
		span.toLine = line
		span.toColumn = column
		return span
	}

	function advance(amount = 1) { lexerIndex += amount }
	function retreat(amount = 1) { lexerIndex -= amount }

	function isNewline(c) { return c == '\n' }

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
		return (c >= '0' && c <= '9')
	}

	function isLegalHexNumber(c) {
		return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
	}

	function isLegalOctalNumber(c) {
		return (c >= '0' && c <= '7')
	}

	function isLegalBinaryNumber(c) {
		return (c == '0' || c == '1')
	}


	const maxTokenLength = 3;
	let lexerIndex = 0, line = 0, column = 0;
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

		const lexSymbol = (from = null, leftEscaped = false) => {
			let rightEscaped = false

			if (!from) {
				if (current() == "'") {
					advance()
					leftEscaped = true
				}
				from = lexerIndex
			}

			while (isLegalSymbol(current())) advance()

			// remove trailing whitespace
			while (isWhitespace(peek(-1))) retreat()


			let symbol = code.slice(from, lexerIndex)

			// symbols may end with ' to prevent collisions with keywords
			if (current() == "'") {
				advance()
				rightEscaped = true
			}

			return { kind: 'symbol', value: symbol, leftEscaped, rightEscaped, span: takeSpan() }
		}

		startSpan()
		let from = lexerIndex
		while (isWhitespace(current())) {
			if (isNewline(current())) {
				// TODO: also keep track of the line number so we can do proper error messages with them :)
				if (tokens.length > 0 && tokens[tokens.length - 1].kind == 'symbol') {
					const smb = tokens[tokens.length - 1]
					// implicit right escape by inserting a newline
					smb.rightEscaped = true
				}

			}
			advance()
		}
		if (from != lexerIndex) {
			tokens.push({ kind: 'whitespace', value: code.slice(from, lexerIndex), span: takeSpan() })
		}

		if (code[lexerIndex] === undefined) break

		startSpan()

		// multi line comment
		if (current() == '/' && peek(1) == '*') {
			let from = lexerIndex + 2
			while (!(current() == '*' && peek(1) == '/')) advance()
			const comment = code.slice(from, lexerIndex)
			advance(2)
			tokens.push({ kind: 'comment', value: comment, span: takeSpan() })
			continue
		}

		// single line comment
		if (current() == '/' && peek(1) == '/') {
			let from = lexerIndex + 2
			while (!(current() == '\n')) lexerIndex++
			const comment = code.slice(from, lexerIndex)
			tokens.push({ kind: 'comment', value: comment, span: takeSpan() })
			continue
		}

		// tag
		if (current() == '#') {
			advance()
			const smb = lexSymbol()
			if (!smb) throw 'expected symbol..'
			const value = smb.value
			tokens.push({ kind: 'tag', value, span: takeSpan() })
			continue
		}

		//string
		let shouldNullTerminate = false
		if (current() == 'c' && peek(1) == '"') {
			shouldNullTerminate = true
			advance()
		}

		if (current() == '"') {
			advance()
			let from = lexerIndex
			while (true) {
				if (current() == '"') break
				if (current() == '\\' && peek(1) == '"') {
					advance(2)
				}
				else {
					advance()
				}
			}

			let str = code.slice(from, lexerIndex)
			// TODO: maybe move this elsewhere
			str = str.replace(/\\n/g, '\n')
				.replace(/\\r/g, '\r')
				.replace(/\\t/g, '\t')
				.replace(/\\0/g, '\0')
				.replace(/\\"/g, '"')
				// arbitrary hex byte
				.replace(/\\x([0-9a-zA-Z]{2})/g, (...p) => String.fromCharCode(parseInt(p[1], 16)))
			if (shouldNullTerminate) str += '\0'
			advance()
			tokens.push({ kind: 'string', value: str, span: takeSpan() })
			continue
		}

		// number
		if (isLegalNumber(current()) || (current() == '.' && isLegalNumber(peek(1)))) {

			function lexNumbers(f) {
				// don't allow underscore at the start
				let allowUnderscore = false
				while (f(current()) || (allowUnderscore && current() == '_')) {
					// don't allow multiple underscores in a row
					allowUnderscore = current() != '_'
					advance()
				}
			}

			function parseNumber(from, to, radix) {
				const slice = code.slice(from, to).replace(/_/g, '')
				return radix ? parseInt(slice, radix) : parseFloat(slice)
			}

			let from = lexerIndex
			if (current() == '0' && peek(1) == 'b') {
				// binary
				advance(2)
				let from = lexerIndex
				lexNumbers(isLegalBinaryNumber)

				let num = parseNumber(from, lexerIndex, 2)
				tokens.push({ kind: 'number', value: num, radix: 2, span: takeSpan() })
				continue
			}
			else if (current() == '0' && peek(1) == 'x') {
				// hex
				advance(2)
				let from = lexerIndex
				lexNumbers(isLegalHexNumber)

				let num = parseNumber(from, lexerIndex, 16)
				tokens.push({ kind: 'number', value: num, radix: 16, span: takeSpan() })
				continue
			}
			else if (current() == '0' && peek(1) == 'o') {
				// octal
				advance(2)
				let from = lexerIndex
				lexNumbers(isLegalOctalNumber)

				let num = parseNumber(from, lexerIndex, 8)
				tokens.push({ kind: 'number', value: num, radix: 8, span: takeSpan() })
				continue

			} else {
				if (current() == '0' && isLegalNumber(peek(1))) {
					console.log(takeSpan())
					assert(false)
				}
				let floating = false
				// decimal
				lexNumbers(isLegalNumber)
				// float
				if (current() == '.') {
					floating = true
					advance()
					lexNumbers(isLegalNumber)
				}

				//										null is treated as parseFloat
				let num = parseNumber(from, lexerIndex, floating ? null : 10)
				tokens.push({ kind: 'number', value: num, radix: 10, floating, span: takeSpan() })
				continue
			}
		}

		// symbols may start with ' to prevent collisions with keywords
		var mightBeKeyword = true
		if (current() == "'" && isLegalSymbol(peek(1))) {
			advance()
			mightBeKeyword = false
		}

		// keyword and symbol
		if (isLegalKeyword(current()) || isLegalSymbol(current())) {
			let from = lexerIndex

			if (mightBeKeyword) {
				while (isLegalKeyword(current())) advance()
				if (current() != "'") {
					let potentialKeyword = code.slice(from, lexerIndex)
					if (keywords.has(potentialKeyword)) {
						tokens.push({ kind: 'keyword', value: potentialKeyword, span: takeSpan() })
						continue
					}
				}
			}

			const value = lexSymbol(from)
			tokens.push(value)
			continue
		}

		// operator
		{
			let from = lexerIndex
			let operatorLen = maxTokenLength
			while (operatorLen) {
				const op = code.slice(from, from + operatorLen)
				if (operators.has(op)) {
					advance(operatorLen)
					tokens.push({ kind: 'operator', value: op, span: takeSpan() })
					continue lex;
				}
				operatorLen--
			}
		}

		throw `unexpected character '${code[lexerIndex]} : ${lexerIndex}'`
	}

	return tokens
}

module.exports = {
	lex,
	operators,
	binaryOperators,
	preUnaryOperators,
	postUnaryOperators,
	assignmentOperators,
}
