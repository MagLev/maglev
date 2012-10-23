*maglev-runtime
detectMismatchWith: other
	^ (self matches: other) ifFalse: [{self. other}]

