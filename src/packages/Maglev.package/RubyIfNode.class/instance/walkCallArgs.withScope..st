*maglev-runtime
walkCallArgs: lst withScope: aScope
  | nargs |
  1 to: (nargs := lst size) - 1 do:[:n |
    (lst at: n) walkWithScope: aScope .
  ].
  nargs ~~ 0 ifTrue:[ | lastarg |
    (lastarg := lst at: nargs) walkWithScope: aScope .
    lastarg postWalkForYield  "does not need to_proc conversion"
  ].
  condition postWalkForYield .  "rcvr does not need to_proc"

