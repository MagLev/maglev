*maglev-runtime
buildTempsOn: aNode
  | start vNames anAssoc aLeaf |
  (start := self numArgs + 1 ) to: (vNames := variableNames) size do:[:n |
	anAssoc := vNames at: n .
	aLeaf := self _leafForVarAssoc: anAssoc at: n .
	aLeaf isTemp ifTrue:[  
       aNode appendTemp: aLeaf . 
     ].
  ].

