*maglev-runtime
walkOptionalArgsRhs: aScope
   | lst |
   lst := list .
   1 to: lst size do:[:n |
     (lst at: n) walkOptionalArgRhs: aScope
   ].

