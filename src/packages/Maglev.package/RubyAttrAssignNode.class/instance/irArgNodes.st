*maglev-runtime
irArgNodes
  | res args sz splatPos rhsIr lim |
  args := self argNodes .
  sz := args size . lim := sz .
  res := { } .
  splatPos := 0 .
  "srcIrNode may already be set from masgn ..." 
  (rhsIr := srcIrNode ) ifNil:[ lim := sz - 1 ]. 
  1 to: lim do:[:n| | anArg |
    anArg := args at: n .
    anArg isSplatNode ifTrue:[ splatPos := n ].
    res add: anArg irArgNode .  
  ].
  res add: (rhsIr ifNil:[ (args at: sz) irLocalAsgnValue ]).
  splatPosition := splatPos .
  irArgNodes := res .
  ^ res

