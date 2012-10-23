*maglev-ast
s_a: aSymbol
  | arr cls sym node |
  arr := (SessionTemps current at:#RubyGlobalNodesDict) at: aSymbol otherwise: nil .
  arr ifNotNil:[ cls := arr at: 2 . sym := arr at: 1] 
         ifNil:[ cls := RubyGlobalVarNode . sym := aSymbol ].
  (node := cls newForRp )
     name: sym .
  ^ node

