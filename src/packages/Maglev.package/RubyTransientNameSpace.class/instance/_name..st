as yet unclassified
_name: aSymbol
  | pns |
  "used in bootstrap only to override smalltalk class names"
  super _name: aSymbol .
  (pns := persistCopy)  ifNotNil:[ pns _name: aSymbol ].
	

