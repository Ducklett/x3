const { tag_int, num, block } = require("./ast")
const { assert } = require("./util")

function evaluate(node) {
	const targetType = node.type
	const value = evaluateRaw(node.run)

	// special case:
	// unlike expression, the if statement conditionally emits a block of declarations
	// we will add a 'use' to the returned block in the parent scope so it can find the symbols
	if (node.run.kind == 'if') {
		if (value) {
			assert(value.scope)
			assert(value.scope.parent)
			value.scope.parent.used.add(value)

			return value
		}
		//  we must have hit an else block that didn't exist
		// emit a dummy block
		return block()
	} else if (typeof value == 'number') {
		assert(targetType.tag == tag_int)
		assert(Math.floor(value) == value)
		return num(value, targetType)
	} else {
		return null
	}
}

function evaluateRaw(node) {
	switch (node.kind) {
		case 'error': return node
		case 'comptime': return evaluateRaw(node.run)
		case 'binary': {
			const left = evaluateRaw(node.a)
			const right = evaluateRaw(node.b)
			// TODO: take type into account
			assert(typeof left == 'number')
			assert(typeof right == 'number')
			switch (node.op) {
				case '+': return left + right
				case '-': return left - right
				case '/': return left / right
				case '*': return left * right
				case '%': return left % right
				case '==': return left == right
				case '!=': return left != right
				case '<=': return left <= right
				case '>=': return left >= right
				case '<': return left < right
				case '>': return left > right
				default: throw `unhandled operator ${node.op}`
			}
		}
		case 'numberLiteral': {
			// TODO: clamp to type
			return node.n
		}
		case 'booleanLiteral': return node.value
		case 'reference': {
			const s = node.symbol
			assert(node.symbol)
			switch (s.kind) {
				case 'declareVar': {
					// TODO: fix constants so we can also force it to be const
					//assert(s.tags.has('const') || s.expr)
					assert(s.expr)
					return evaluateRaw(s.expr)
				}
				default: throw `unhandled symbol kind ${s.kind}`
			}
		}
		case 'call': {
			assert(node.def.symbol.name == 'assert', `can only call assert at compile time for now`)
			assert(node.args.length == 3)
			const [condition, message, callsite] = node.args
			const success = evaluateRaw(condition)

			if (!success) {
				console.log(`${callsite.value} assertion failed: ${message.value}`)
				process.exit(1)
			}
			return null
		}
		case 'if': {
			const condition = evaluateRaw(node.cond)
			assert(typeof condition == 'boolean')
			if (condition) return node.then
			if (!node.els) return null
			if (node.els.kind == 'if') return evaluateRaw(node.els)

			assert(node.els.kind == 'block')
			return node.els
		}
		default:
			console.log(node)
			throw `unhandled node in evaluator: ${node.kind}`
	}
}

module.exports = { evaluate, evaluateRaw }
