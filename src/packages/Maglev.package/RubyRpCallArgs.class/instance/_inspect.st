*maglev-runtime
_inspect
  | res |
  res := '[:callargs ' copy .
  list ifNotNil:[ res add: ', ' ; add: self _inspect_list ].
  iterNode ifNotNil:[ res add: ', <iterNode>' ; add: iterNode _inspect ].
  res add: $] .
  ^ res

