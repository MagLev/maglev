*maglev-runtime
irDefinedQNode_aCall
  "generate code to return either  'method'  or  nil   at runtime"
  | send sel rcvrBlk |
  sel := self methodName .
  rcvrBlk := self newBlock:[:blk | blk appendStatement: self irReceiverNode . blk ].
  (send := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: RubyConstantRef ) ;
     stSelector:  #_abstractCall:definedQ:  ;
     appendArgument: rcvrBlk ;
     appendArgument: ( GsComLiteralNode newObject: sel ) .
  self ir: send .
  ^ send 

