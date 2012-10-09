*maglev-runtime
irForDynamicCDecl: valIrNode
    |  node aNameNode  |   
    (aNameNode := RubySymbolNode newForIr) name: name .
    (node := GsComSendNode new) 
       rcvr:  (GsComLiteralNode newObject: Object  ) ;
       stSelector:   #rubyConstAt:put:   ;
       appendArgument:  aNameNode irNode ;
       appendArgument:  valIrNode .
    self ir: node .
    ^ node 

