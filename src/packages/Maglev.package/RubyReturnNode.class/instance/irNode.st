converting
irNode
	| value |  
	value := valueNode ifNil: [ GsComLiteralNode newNil ] 
	                  ifNotNil: [valueNode irReturnNode ].
	^ self ir: (GsComReturnNode new returnFromHome: value)

