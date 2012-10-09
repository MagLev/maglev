*maglev-runtime
irForDynamicCDecl: valIrNode
    |  node  lhs  | 
    dynamicTypeError ifNotNil:[ self error: 'Colon2.irForDynamicCDecl - unexpected dynamicTypeError' ].
    node := GsComSendNode new .
    (lhs := leftNode) ifNil:[  | cst topMod |
      cst := RubyCompilerState current .
      topMod := cst topRtModule .
      node
        rcvr: (topMod == Object ifTrue:[ GsComLiteralNode newObject: topMod ] 
                    ifFalse:[ GsComVariableNode newSelf ] ); 
        stSelector:   #rubyConstDecl:put: ;
        appendArgument:  (GsComLiteralNode newObject: name) "a Symbol" ;
        appendArgument:  valIrNode .  "no longer using lexPath"
    ] ifNotNil:[
      node 
        rcvr:    lhs  irNode "a Colon2Node" ;
        stSelector:  #rubyConstAt:put:  ;
        appendArgument:  (GsComLiteralNode newObject: name) "a Symbol" ;
        appendArgument:  valIrNode .
    ].
    self ir: node .
    ^ node 

