(as yet unclassified)
walkWithScope: aScope
   | lst |
   lst := list .
   1 to: lst size do:[:n |
     (lst at: n) walkWithScope: aScope
   ].

