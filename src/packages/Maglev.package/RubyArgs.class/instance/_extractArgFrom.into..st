as yet unclassified
_extractArgFrom: argsArray into: anArray
	"Return the parameter to a command line argument that takes the
	 form  '-I' 'param'  or '-Iparam', and return 'param'.
	 Signal an error if param is missing.
	 Modifies argsArray to consume parameters."
	
	|arg sz result|
	arg := argsArray removeFirst .
	(arg beginsWith: '-') 
		ifFalse:[ self error: 'parameter should begin with - : ', arg printString ] .
	sz := arg size .
	result :=
		sz = 2 
			ifTrue:[ 
				argsArray isEmpty ifTrue:[ self error: arg printString, ' needs a parameter' ] .
				argsArray removeFirst ]
			ifFalse:[ 
				sz < 3 ifTrue:[ self error: arg printString, ' needs a parameter' ].
				arg copyFrom: 3 to: sz ] .
	anArray add: result .
	^ result .

