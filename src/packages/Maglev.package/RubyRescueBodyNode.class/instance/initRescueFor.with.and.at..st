initialization
initRescueFor: exList with: body and: nextRescue at: srcOfs 
	| condition orNodeBlk nextRes lastEx |
	orNodeBlk := self exceptionKindOfBlk.
	nextRes := nextRescue.
	lastEx := exList list last.
	lastEx class == RubySplatNode
		ifTrue: [
			exList list size < 2 ifTrue: [
				RubyParserM signalError:'invalid singular splat in multi-rescue args for RubyRescueBodyNode'].
			nextRes := RubyRescueBodyNode s_a: lastEx b: body c: nextRes d: srcOfs.
			exList list removeLast.
			lastEx := exList list last].
	condition := exList list allButLast reverse
					inject: (orNodeBlk value: lastEx)
					into: [:rhs :lhs | RubyOrNode s_a: (orNodeBlk value: lhs) b: rhs].
	self initRescue: condition with: body and: nextRes at: srcOfs.
