as yet unclassified
irDefinedQNode
  "should only be used when generating bootstrap code"
  | node |
  (node := GsComSendNode new)
     stSelector:  #definedQ  ;
     rcvr: ( GsComLiteralNode newObject: self )  .
   ^ node 

