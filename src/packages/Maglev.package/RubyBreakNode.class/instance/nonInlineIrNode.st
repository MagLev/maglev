*maglev-runtime
nonInlineIrNode
   | node  |            
   (node := GsComSendNode new)
      rcvr: (GsComLiteralNode newObject: RubyBreakException ) ;
      stSelector:  #signalBreakWith:  ;
      appendArgument: self irGotoValueNode .
   self ir: node .
   ^ node .

