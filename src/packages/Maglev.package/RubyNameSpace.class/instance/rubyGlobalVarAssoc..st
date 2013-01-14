*maglev-runtime
rubyGlobalVarAssoc: aSymbol
  " for transient name spaces only"
  | assoc |
  (assoc := self _at: aSymbol otherwise: nil) ifNil:[  
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    assoc globalVarValue: ( self initialValueForGlobal: aSymbol )  .
    self addAssociation: assoc .
  ].
  ^ assoc 

