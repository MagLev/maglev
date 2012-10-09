as yet unclassified
resolveConstant: aSymbol
  "runtime constant resolution, returns a RubySymbolAssociation or nil"
  | assoc pns |
  assoc := self _at: aSymbol otherwise: nil .
  assoc ifNil:[
    (pns := persistCopy ) ifNotNil:[ assoc := pns _at: aSymbol otherwise: nil ].
  ].
  ^ assoc

