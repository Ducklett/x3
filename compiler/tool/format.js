const { assert } = require("../emitter")

module.exports = {
	format(syntaxtree, marker) {
		class Marked {
			constructor(id, it) {
				this.it = it
				this.id = id
			}

			toString() {
				if (marker) return marker(this.id, this.it)
				else return this.it
			}
		}

		// return "marked" objects,
		// higher level nodes can change the meaning of the symbol,
		// that just changes the id.
		// when it finally gets converted into a string the toString() method is called which applies the marker
		//
		// we need this because otherwise the marker would be called twice in some places
		function mark(id, it) {
			if (it instanceof Marked) {
				it.id = id
				return it
			} else {
				return new Marked(id, it)
			}
		}

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
		function _formatList(nodes) {
			return nodes.map(_format).join('\n')
		}
		function escapeString(str) {
			return str
				.replace(/\n/g, '\\n')
				.replace(/\r/g, '\\r')
				.replace(/\0/g, '\\0')
				.replace(/\t/g, '\\t')

		}
		function _format(node) {
			if (node === undefined || node === null) return ''
			switch (node.kind) {
				case 'file': return eachOnNewLine(node.declarations.map(_format))
				case 'tag': return join([
					mark('comment', `#${node.value}`),
					node.list ? _format(node.list) : ''
				])
				case 'import': {
					return separatedBySpace([
						_format(node.keyword),
						_format(node.path),
					])
				}
				case 'use': {
					return separatedBySpace([
						_format(node.keyword),
						mark('symbol-const', _format(node.path))
					])
				}
				case 'module': {
					const str = []

					formatTags(str, node.tags)

					str.push(_format(node.keyword))
					str.push(mark('symbol-const', _format(node.name)))
					str.push(_format(node.block))
					return separatedBySpace(str)
				}
				case 'do': {
					return join([
						formatTags(node.tags),
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
						space(),
						_format(node.body)
					])
				}
				case 'proc': {
					const hasBody = node.body
					return join([
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
						space(hasBody ? 1 : 0),
						formatTags(node.tags, hasBody ? 1 : 0),
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
							space(),
							_format(node.expr),
							_format(node.terminator),
						]) : '',
					])
				}
				case 'type alias': {
					return join([
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
				case 'goto': {
					return separatedBySpace([
						_format(node.keyword),
						mark('symbol-const', _format(node.label))
					])
				}
				case 'label': {
					return separatedBySpace([
						_format(node.keyword),
						mark('symbol-const', _format(node.label)),
						_format(node.colon),
					])
				}
				case 'if': {
					return join([
						_format(node.keyword),
						space(),
						_format(node.condition),
						space(),
						_format(node.thenBlock),
						_format(node.elseKeyword),
						_format(node.elseBlock),
					])
				}
				case 'break': return _format(node.keyword)
				case 'continue': return _format(node.keyword)
				case 'while': {
					return join([
						_format(node.keyword),
						space(),
						_format(node.condition),
						space(),
						_format(node.block),
					])
				}
				case 'for': {
					return join([
						_format(node.keyword),
						space(),
						_format(node.begin),
						_format(node.preCondition),
						_format(node.terminator1),
						space(),
						_format(node.condition),
						_format(node.terminator2),
						space(),
						_format(node.postCondition),
						_format(node.end),
						space(),
						_format(node.block),
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
				case 'parenthesized expression': {
					return join([
						_format(node.open),
						_format(node.expr),
						_format(node.close),
					])
				}
				case 'offset access': {
					return join([
						_format(node.name),
						_format(node.begin),
						_format(node.index),
						_format(node.end),
					])
				}
				case 'post unary': {
					return join([
						_format(node.expr),
						_format(node.op),
					])
				}
				case 'pre unary': {
					return join([
						_format(node.op),
						_format(node.expr),
					])
				}
				case 'call': {
					const str = []
					str.push(mark('symbol-code', _format(node.name)))
					str.push(_format(node.argumentList))
					return join(str)
				}
				case 'assignment': {
					const str = []
					return separatedBySpace([
						_format(node.name),
						_format(node.operator),
						_format(node.expr),
					])
				}
				case 'property access': {
					const str = []
					// str.push(mark('symbol-const', _format(node.name)))
					str.push(_format(node.scope))
					str.push(_format(node.dot))
					str.push(_format(node.property))
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

					str.push(_format(node.name))
					str.push(_format(node.colon))
					str.push(_format(node.type))
					return join(str)
				}
				// its color will change depending on context, the parent will color it
				case 'symbol': return mark('symbol', node.value)
				case 'operator': return mark(pseudoOperators.has(node.value) ? 'pseudo-operator' : 'operator', node.value)
				case 'number': return mark('number', node.value)
				case 'string': return mark('string', `"${escapeString(node.value)}"`)
				case 'boolean literal': return mark('bool', `${node.value}`)

				// ========== types ==========
				case 'type atom': return _format(node.name)
				case 'type mutable': {
					return join([
						_format(node.mutable),
						_format(node.it)
					])
				}
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

				default: throw `unhandled node kind in formatter ${JSON.stringify(node.kind)}`
			}
		}

		assert(Array.isArray(syntaxtree))
		return _formatList(syntaxtree)
	}
}
