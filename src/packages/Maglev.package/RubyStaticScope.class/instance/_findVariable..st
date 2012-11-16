*maglev-runtime
_findVariable: aSymbol 
  |  anAssoc vNames  |
  1 to: (vNames := variableNames) size do:[:j |
	(anAssoc := vNames at: j) key == aSymbol ifTrue:[  ^ anAssoc ]
  ].
  ^ nil  

