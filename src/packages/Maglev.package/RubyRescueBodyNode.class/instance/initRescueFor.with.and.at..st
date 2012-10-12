initialization
initRescueFor: exList with: body and: nextRescue at: srcOfs 
	| condition orNodeBlk |
	orNodeBlk := self exceptionKindOfBlk.
	condition := exList list allButLast reverse
					inject: (orNodeBlk value: exList list last)
					into: [:rhs :lhs | RubyOrNode s_a: (orNodeBlk value: lhs) b: rhs].
	self initRescue: condition with: body and: nextRescue at: srcOfs.
