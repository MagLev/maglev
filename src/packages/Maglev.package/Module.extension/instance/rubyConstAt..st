*maglev-runtime
rubyConstAt: aName
  "a ruby primitive"
    "Runtime support for a dynamic Colon2Node, searches superClasses "
  | assoc sym envId |
  (sym := aName) _isSymbol ifFalse:[
    sym _isOneByteString ifFalse:[
       aName ifNil:[ ArgumentTypeError signal:'left side of :: is not a class/module ' ]
               ifNotNil:[ ArgumentTypeError signal:'expected a String or Symbol'].
    ].
    sym := aName asSymbol .
  ].
  assoc := self rubyConstAssociationAtOrNil: sym env: (envId := 1"__callerEnvId").
  assoc ifNotNil:[ ^ assoc _valueFor: sym inClass: self env: envId  ].
  self == Kernel ifTrue:[ assoc := Object rubyConstAssociationAtOrNil: sym env: envId ].
  assoc ifNotNil:[ ^ assoc _valueFor: sym inClass: self env: envId  ]
        ifNil:[ ^ self @ruby1:const_missing: sym ]

