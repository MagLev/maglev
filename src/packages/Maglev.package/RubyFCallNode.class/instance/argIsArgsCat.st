accessing
argIsArgsCat
  | n |
  ^ (n := argsNode) ifNotNil:[ n isArgsCatNode] ifNil:[ false ]

