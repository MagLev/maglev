*maglev-runtime
irTargetNode
   | node |
   outerDef ifNotNil:[ :o | "within an eval or an outer def"
     (node := GsComSendNode new)
        rcvr: ( GsComLiteralNode newObject: GsProcess ) ;
        stSelector: #currentMethDefTarget .
   ] ifNil:[
     (node := GsComSendNode new) 
       rcvr: ( GsComLiteralNode newObject: GsProcess ) ;
       stSelector:  #methodDefnTarget .
   ].
   self ir: node .
   ^ node

