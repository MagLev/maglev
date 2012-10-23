*maglev-runtime
parent
  | p |
  (p := parent) ifNotNil:[ ^ p ].
  (p := persistCopy)  ifNotNil:[ ^ p parent ].
  ^ nil

