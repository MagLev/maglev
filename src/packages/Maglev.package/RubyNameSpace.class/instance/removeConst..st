*maglev-runtime
removeConst: aSymbol
  | assoc  |
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  RubyCompilerState current persistenceMode ifTrue:[
     assoc := self _removeKey: aSymbol ifAbsent:[ nil ].
  ] ifFalse:[
    assoc := self associationAt: aSymbol otherwise: nil .
    assoc ifNotNil:[
      ArgumentTypeError signal:'cannot remove constant from persistent module in transient mode'.
    ]
  ].
  assoc ifNil:[
     NameError signal: 'remove_const, ' , aSymbol , ' not found '
  ].
  ^ assoc _valueNoAction

