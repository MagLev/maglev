*maglev-runtime
at: aSymbol classVarPut: aValue 
  | assoc pns |
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
assoc := self _at: aSymbol otherwise: nil .    "aSymbol == #Y ifTrue:[ self pause ]."
assoc ifNotNil:[ "found in tns"
  assoc _value: aValue  "store to tns"
] ifNil:[   | newAssoc |
  newAssoc := RubyClassVarAssociation newWithKey: aSymbol .
  newAssoc _value: aValue .
  self addAssociation: newAssoc 
].

^ aValue

