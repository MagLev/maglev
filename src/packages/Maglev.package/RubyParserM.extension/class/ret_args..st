*maglev-ast
ret_args: aCallArgs
  | lst res |
  aCallArgs iter ifNotNil:[ 
    self signalError:'block argument should not be given'
  ].
  res := aCallArgs .
  (lst := res list ) size == 1 ifTrue:[ res := lst at: 1 ].
  ^ res

