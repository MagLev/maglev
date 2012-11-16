(as yet unclassified)
buildIrLeafsInto: anArray
  | lst |
  lst := list .
  1 to: lst size do:[:n |
    (lst at: n) buildIrLeafsInto: anArray
  ].

