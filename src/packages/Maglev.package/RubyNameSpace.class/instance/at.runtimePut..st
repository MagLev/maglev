*maglev-runtime
at: aSymbol runtimePut: aValue 
  | assoc pns |
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
assoc := self _at: aSymbol otherwise: nil .    "aSymbol == #Y ifTrue:[ self pause ]."
assoc ifNotNil:[ 
  assoc _value: aValue  
] ifNil:[   | newAssoc |
  newAssoc := RubySymbolAssociation newWithKey: aSymbol .
  newAssoc _value: aValue .
  self addAssociation: newAssoc 
].
^ aValue

