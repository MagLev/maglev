*maglev-ast
_append: aNode
  | lst |
  (lst := list) == #() ifTrue:[
    list := { aNode }
  ] ifFalse:[
    lst add: aNode
  ] .
  ^ self

