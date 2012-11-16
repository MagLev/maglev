as yet unclassified
useScope: aScope during: aBlock
    
	^ self nextLexLevel: [:i |  | scopeStk res |
		scopeStk := RubyCompilerState current scopeStack .
		scopeStk push: aScope .
		[
		  res:= aBlock value 
		] ensure: [
		   scopeStk pop: aScope
		].
	     res
	  ]
		

