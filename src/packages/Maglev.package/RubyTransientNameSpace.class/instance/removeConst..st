as yet unclassified
removeConst: aSymbol
  | assoc pns passoc |
  moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to modify a frozen module/class'].
  assoc := self _removeKey: aSymbol ifAbsent:[ nil ].
  (pns := persistCopy) ~~ nil ifTrue:[
     RubyCompilerState current persistenceMode ifTrue:[
       passoc := pns _removeKey: aSymbol ifAbsent:[ nil ].
       assoc ifNil:[ assoc := passoc ].
     ] ifFalse:[
       passoc := pns associationAt: aSymbol otherwise: nil .
       passoc ifNotNil:[
         "ArgumentTypeError signal:'cannot remove constant from persistent module in transient mode'."
       ]
     ]
  ].
  assoc ifNil:[
     NameError signal: 'remove_const, ' , aSymbol , ' not found '
  ].
  ^ assoc _valueNoAction

