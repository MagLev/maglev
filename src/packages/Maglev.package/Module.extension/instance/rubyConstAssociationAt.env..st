*maglev-runtime
rubyConstAssociationAt: aSymbol env: envId
  | assoc val  |
  name == #'' ifTrue:[ NameError signal:'empty NameSpace for ', self name ] .

  assoc := self rubyConstAssociationAtOrNil: aSymbol env: envId.
  assoc ifNil:[
    val := self @ruby1:const_missing: aSymbol .
    (assoc := RubySymbolAssociation newWithKey: aSymbol) _value: val .
  ].
  ^ assoc

