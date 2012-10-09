*maglev-ast
as_yield_args
  | lst nod |
  ((lst := list) size == 1 and:[ iterNode == nil ]) ifTrue:[
     (nod := lst at: 1) class == RubySplatNode ifTrue:[
       ^ nod " for   yield *one_arg"
     ]
  ].
  ^ self

