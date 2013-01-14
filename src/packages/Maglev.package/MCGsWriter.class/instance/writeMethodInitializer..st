as yet unclassified
writeMethodInitializer: aMethodDefinition

	aMethodDefinition isInitializer ifTrue:
		[(self class: aMethodDefinition className)
			cr;
			nextPutAll: 'doit'; cr;
			nextPutAll: aMethodDefinition className, ' initialize.'; cr;
			nextPutAll: '%'; cr; cr].

