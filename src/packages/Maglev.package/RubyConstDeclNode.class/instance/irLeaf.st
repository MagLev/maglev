(as yet unclassified)
irLeaf
  isDynamic ifTrue:[ self error:'illegal irLeaf for dynamic ConstDecl'].
  globalAssoc ifNil:[ self error:'unresolved global during IR'].
  ^ self irLitVarLeaf: globalAssoc 

