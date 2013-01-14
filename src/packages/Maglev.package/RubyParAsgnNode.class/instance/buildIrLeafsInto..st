as yet unclassified
buildIrLeafsInto: anArray
  | lList lsiz |
  rightList ifNotNil:[ self error:'invalid RHS on ParAsgn as for loop args '].
  lList := leftList .
  1 to: lList size do:[:n | (lList at: n) buildIrLeafsInto: anArray  ].

