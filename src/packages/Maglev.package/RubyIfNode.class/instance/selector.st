converting
selector
	^ elseBody ifNil: [ #ifTrue: ] 
	        ifNotNil: [  thenBody ifNil: [ #ifFalse: ] ifNotNil: [ #ifTrue:ifFalse: ] ]

