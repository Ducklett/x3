const { tag_int, num } = require("./ast")
const { assert } = require("./util")

function evaluate(node) {
	const targetType = node.type
	const value = evaluateRaw(node)
	assert(typeof value == 'number')
	assert(targetType.tag == tag_int)
	assert(Math.floor(value) == value)
	return num(value, targetType)
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
				default: throw `unhandled operator ${node.op}`
			}
			assert(false)
		}
		case 'numberLiteral': {
			// TODO: clamp to type
			return node.n
		}
		default:
			console.log(node)
			throw `unhandled node in evaluator ${node.kind}`
	}
}

module.exports = { evaluate, evaluateRaw }
