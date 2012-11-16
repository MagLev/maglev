*maglev-runtime
irNodeListInto: blockIr
  | lst |
  lst := list .
  1 to: lst size do:[:n | (lst at: n) irNodeListInto: blockIr ].

