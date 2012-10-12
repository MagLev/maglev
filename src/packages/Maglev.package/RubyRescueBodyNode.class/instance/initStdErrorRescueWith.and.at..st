initialization
initStdErrorRescueWith: body and: nextRescue at: srcOfs 
	| condition |
	condition := self exceptionKindOfBlk value: (RubyColon2Node sym: #StandardError srcOffset: srcOfs).
	self initRescue: condition with: body and: nextRescue at: srcOfs.