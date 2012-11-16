as yet unclassified
absolutePathFor: aString
	"If the first character of aString is not '/', then prepend the current working 
	directory and return the path. Does NOT compress embedded occurrences of '.' and '..'."
	|cwd|
	(aString beginsWith: '/') ifTrue: [ ^ aString ].
	^ RubyDirectory _getwd, '/' , aString .
	

