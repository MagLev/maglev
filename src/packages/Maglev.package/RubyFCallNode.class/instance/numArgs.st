converting
numArgs
	argsNode ifNil: [^ 0].
	argsNode list ifNil: [^ 0].
	^ argsNode list size

