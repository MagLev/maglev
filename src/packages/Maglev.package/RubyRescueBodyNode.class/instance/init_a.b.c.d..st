*maglev-ast
init_a: ex_list b: body c: next_rescue d: srcOfs 
	| condition elseNode orNodeBlk |
	orNodeBlk := [:expr | RubyCallNode
							s_a: (RubyGlobalLastException s_a: #'$!')
							b: #'_kind_of?'
							c: (RubyRpCallArgs s_a: expr)].
	ex_list ~= nil "create or'ed kind_of? calls for each expression in ex_list"
		ifTrue: [condition := ex_list list allButLast reverse
					inject: (orNodeBlk value: ex_list list last)
					into: [:rhs :lhs | RubyOrNode s_a: (orNodeBlk value: lhs) b: rhs]]
		ifFalse: [condition := orNodeBlk value: (RubyColon2Node sym: #StandardError srcOffset: srcOfs)].

	next_rescue ~= nil
		ifTrue: [elseNode := next_rescue bodyNode].
	bodyNode := RubyIfNode s_a: condition b: body c: elseNode.
	exceptionNodes := RubyArrayNode _new: (RubyColon2Node sym: #Exception srcOffset: srcOfs).
	self position: srcOfs.
