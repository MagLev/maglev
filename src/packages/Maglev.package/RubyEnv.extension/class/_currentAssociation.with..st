*maglev-runtime
_currentAssociation: aKey with: aBlock
   | tempsDict assoc   |
assoc := (tempsDict := SessionTemps current) associationAt: aKey otherwise: nil .
assoc ifNil:[  | val |
  val := aBlock value.
  assoc := RubySymbolAssociation newWithKey: aKey .
  assoc _value:  aBlock value .
  tempsDict addAssociation: assoc .
].
^ assoc

