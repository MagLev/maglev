as yet unclassified
setIsBlockArg
  | lList lsiz |
  lList := leftList .
  1 to: lList size do:[:n | (lList at: n) setIsBlockArg ].

