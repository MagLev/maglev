*maglev-runtime
walkOptionalArgsLhs: aScope
   | lst |
   lst := list .
   1 to: lst size do:[:n |
     (lst at: n) walkOptionalArg: aScope
   ].

