*maglev-runtime
irNode
  | callNode node assoc  |  
  (assoc := globAssoc) ifNotNil:[
    assoc class == RubyConstantRef ifFalse:[
      ^ super irNode   "an association bound at compile time, in bootstrap"
    ].
  ].
  "assoc is an instance of RubyConstantRef from walkWithScope"
  (node := GsComSendNode new)
     stSelector:  #resolveConst  ;
     rcvr: (GsComLiteralNode newConstantRef: assoc ) .
  self ir: node .
  ^ node

