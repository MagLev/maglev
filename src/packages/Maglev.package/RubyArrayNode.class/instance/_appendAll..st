*maglev-ast
_appendAll: anArrayNode
  | lst args |
  args := anArrayNode list .
  args size ~~ 0 ifTrue:[
    (lst := list) == #() ifTrue:[
      list := args copy
    ] ifFalse:[ 
      lst addAll: args
    ]
  ].
  ^ self

