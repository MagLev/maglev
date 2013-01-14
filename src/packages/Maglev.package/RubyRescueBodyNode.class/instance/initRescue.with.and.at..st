initialization
initRescue: condition with: body and: nextRescue at: srcOfs
	| elseNode |
	nextRescue ~= nil
		ifTrue: [elseNode := nextRescue bodyNode]
		ifFalse: [elseNode := RubyVCallNode s_a: (RubyGlobalLastException s_a: #'$!') b: #'__reraise'].
	bodyNode := RubyIfNode s_a: condition b: body c: elseNode.
	exceptionNodes := RubyArrayNode _new: (RubyColon3Node _basicNew name: #Exception; yourself).
	self position: srcOfs.
