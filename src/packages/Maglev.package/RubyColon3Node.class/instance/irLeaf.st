(as yet unclassified)
irLeaf
  globAssoc ifNil:[ self error:'unresolved global during IR'].
  ^ self ir: (self irLitVarLeaf: globAssoc )

