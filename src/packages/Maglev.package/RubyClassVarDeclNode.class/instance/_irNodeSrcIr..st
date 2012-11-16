*maglev-runtime
_irNodeSrcIr: srcIr 
  |  node |
  (node := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     stSelector:  #addRubyClassVar:value:mref:   ;
     appendArgument: (GsComLiteralNode newObject: name ) ;
     appendArgument:  srcIr  ;
     appendArgument: (inMethod ifTrue:[ GsComLiteralNode newObject: 
                                          RubyCompilerState current topRtModule
                                   ]
                                    ifFalse:[ GsComVariableNode newSelf ] ) .
  ^ self ir: node 

