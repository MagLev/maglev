*maglev-runtime
irDefinedQNode
  "maybe not used ?"
  | node |
  (node := GsComSendNode new)
       stSelector:  #definedQconst ;
       rcvr: (GsComLiteralNode newObject: self  ) .
  ^ node

