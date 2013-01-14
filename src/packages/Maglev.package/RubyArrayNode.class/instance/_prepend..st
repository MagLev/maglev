*maglev-ast
_prepend: aNode
  | lst |
  (lst := list) == #() ifTrue:[
     list := { aNode }
  ] ifFalse:[
    lst insertAll: { aNode } at: 1
  ] .
  ^ self 

