*maglev-ast
_append: aNode
  | lst |
  (lst := list) ifNil:[
    list := { aNode }
  ] ifNotNil:[
    lst add: aNode
  ] .
  ^ self

