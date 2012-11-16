*maglev-runtime
rubyGlobalVar: aSymbol put: aValue
  " for transient name spaces only. returns an Association"
  | assoc |
  (assoc := self _at: aSymbol otherwise: nil) ifNil:[
     assoc := RubyGlobalVarAssociation newWithKey: aSymbol .
     self addAssociation: assoc .
  ].
  assoc globalVarValue: aValue .
  ^ assoc 

