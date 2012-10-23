*maglev-runtime
walkOptionalArgs: aScope
   | lst sz |
   lst := list .
   1 to: (sz := lst size) do:[:n |
     (lst at: n) walkOptionalArg: aScope
   ].
   1 to: sz do:[:n |
     (lst at: n) walkOptionalArgRhs: aScope
   ].

