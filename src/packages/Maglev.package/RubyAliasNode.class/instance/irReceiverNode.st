*maglev-runtime
irReceiverNode 
  | node |
    (node := GsComSendNode new) 
       rcvr: ( GsComLiteralNode newObject: GsProcess ) ;
       stSelector:  #methodDefnTarget .
    self ir: node .
    ^ node 

