*maglev-runtime
leafForVarAssoc: anAssoc 
  | ofs aLeaf  nA |
  ofs := variableNames indexOfIdentical: anAssoc .
  ofs == 0 ifTrue:[ self error: ' assoc not found for leaf' ].
  ^ self _leafForVarAssoc: anAssoc at: ofs 

