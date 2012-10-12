initialization
initStdErrorRescueWith: body and: nextRescue at: srcOfs 
	| condition |
	condition := self exceptionKindOfBlk value: (RubyColon3Node _basicNew name: #StandardError; yourself).
	self initRescue: condition with: body and: nextRescue at: srcOfs.