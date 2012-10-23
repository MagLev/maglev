*maglev-runtime
rubyConstAt: sym env: envId
  "called from smalltalk code"
  | assoc |
  sym _isSymbol ifFalse:[ ArgumentTypeError signal:'expected a Symbol'].
  assoc := self rubyConstAssociationAtOrNil: sym env: envId .
  ^ assoc ifNotNil:[  assoc _valueFor: sym inClass: self env: envId  ]
        ifNil:[ self @ruby1:const_missing: sym ]

