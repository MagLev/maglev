*maglev-runtime
parentFor: aName
  "Returns a scope for specified name, 
   or returns nil if caller should start search at top scope using aName.
   used during compile time constant resolution for bootstrap code."
  | ns sym  |
  sym := aName asSymbol .
  ns := self parent .
  [ ns ~~ nil ] whileTrue:[ | val next |
    val := ns _childScopeAt: sym .
    val ifNotNil:[ ^ val  ].
    ns := ns parent .
  ].
  ^ nil

