*maglev-runtime
leafForVarInfo: anInfo 
  | ofs aLeaf  nA |
  ofs := variableNames indexOfIdentical: anInfo .
  ofs == 0 ifTrue:[ self error: ' VarInfo not found for leaf' ].
  ^ self _leafForVarAssoc: anInfo at: ofs 

