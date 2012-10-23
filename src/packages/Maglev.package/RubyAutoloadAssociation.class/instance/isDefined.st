*maglev-runtime
isDefined
	"If we haven't tried to load from the file, then we should consider
	 ourselves as defined.  After we have tried to load the file, then
	 we are defined or not based on the results of that load."
	isLoaded ifNil: [ ^ true ] . "We are in the process of loading"
	^isLoaded ifTrue: [ isDefined ]
	          ifFalse: [ true ] .
	

