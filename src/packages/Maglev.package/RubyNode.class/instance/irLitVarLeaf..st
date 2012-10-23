as yet unclassified
irLitVarLeaf: anAssoc
  | val assoc |
  assoc := anAssoc .
  (assoc isKindOf: RubySymbolAssociation) ifFalse:[ self error:'invalid association'].
  ^ GsComVarLeaf new literalVariable:  assoc

