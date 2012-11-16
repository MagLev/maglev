as yet unclassified
indexOfInstVar: aSymbol with: aNode
   "aSymbol is an instVar name without leading $@ "
	|  index  cls names |
	names := (cls := currentClass) _instVarNames .
	index := names ifNil:[ 0] ifNotNil: [ names indexOfIdentical: aSymbol ] .
	index == 0 ifTrue:[
		cls isModifiable_oops_notModule ifTrue:[
				cls addRubyInstVar: aSymbol .
				index := currentClass _instVarNames indexOfIdentical: aSymbol ]
			ifFalse:[ index := -1 . "a dynamic instVar" ]
	] ifFalse:[  | privateSz |
	  ((privateSz := cls rubyPrivateInstSize) > 0 and:[ index <= privateSz 
	           and:[  installingPrims not]]) ifTrue:[
		  aNode signalParseError: ' in class ',  cls name , ' instVar ' , aSymbol , ' is private to Smalltalk'
	   ].
	].
	^ index

