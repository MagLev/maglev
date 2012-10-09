as yet unclassified
leafForInstVar: stName rubyName: rubyName with: aRubyNode
  | sym |
  sym := stName ifNil:[ rubyName ].
	^ leaves at: sym  ifAbsentPut:      "dictionary keyed on smalltalk style names"
		[ | ivOfs  |
		  ivOfs := self indexOfInstVar: sym with: aRubyNode .
		  ivOfs == -1 ifTrue:[
		    installingPrims ifTrue:[
			  aRubyNode signalParseError:'dynamic instVar ', sym ,
			     '  not allowed in bootstrap code ' 
			].
		  ].
		  GsComVarLeaf new
			instanceVariable: sym   
			ivOffset:  ivOfs
		]

