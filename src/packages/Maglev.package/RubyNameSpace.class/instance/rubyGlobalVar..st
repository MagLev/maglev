*maglev-runtime
rubyGlobalVar: aSymbol
  " for transient name spaces only"
  | assoc |
  (assoc := self _at: aSymbol otherwise: nil) ifNil:[ | val |
    assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
    assoc globalVarValue: ( val := self initialValueForGlobal: aSymbol )  .
    self addAssociation: assoc .
    ^ val
  ] ifNotNil:[
    ^ assoc globalVarValue
  ]

