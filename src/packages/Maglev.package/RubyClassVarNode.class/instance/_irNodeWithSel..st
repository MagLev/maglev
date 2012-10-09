*maglev-runtime
_irNodeWithSel:  runtimeSel  
  |  node  |
  (node := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     stSelector: runtimeSel   ;
     appendArgument: (GsComLiteralNode newObject: name ) ;
     appendArgument: ( inMethod ifTrue:[ GsComLiteralNode newObject: 
                                          RubyCompilerState current topRtModule
                           ]
                           ifFalse:[  GsComVariableNode newSelf ] ) .
  ^ self ir: node

