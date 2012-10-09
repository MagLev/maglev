*maglev-ast
s_a: aSymbol b: srcOfs c: val
  | arr cls node sym |
  arr := (SessionTemps current at:#RubyGlobalNodesDict) at: aSymbol otherwise: nil .
  arr ifNotNil:[ cls := arr at: 3 . sym := arr at: 1] 
         ifNil:[ cls := RubyGlobalAsgnNode . sym := aSymbol ].
  (node := cls newForRp )
      name: sym ; valueNode: val ; position: srcOfs .
  ^ node

