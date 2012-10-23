as yet unclassified
at: aSymbol transientRuntimePut: aValue 
| assoc pns |
moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
assoc := self _at: aSymbol otherwise: nil .
assoc ifNotNil:[ "found in tns"
  assoc _value: aValue  "store to tns"
] ifNil:[   | newAssoc |
  newAssoc := RubySymbolAssociation newWithKey: aSymbol .
  newAssoc _value: aValue .
  self addAssociation: newAssoc 
].
^ aValue

