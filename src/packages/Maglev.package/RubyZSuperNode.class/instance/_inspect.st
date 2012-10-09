*maglev-runtime
_inspect
  | res |
  res := '[:zsuper ' copy.
  iterNode ifNotNil:[
    res add: ', '; add: iterNode _inspect 
  ].
  res add: $] .
  ^ res

