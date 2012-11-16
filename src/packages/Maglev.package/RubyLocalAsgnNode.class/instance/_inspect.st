*maglev-runtime
_inspect
  | res |
  res := '[:lasgn, ' copy .
  hasAmpersand ifNotNil:[ res add: $& ].
  res add: ' :'; add: name .
  valueNode ifNotNil:[ res addAll: ', ', valueNode _inspect ].
  res add: $]  .
  ^ res

