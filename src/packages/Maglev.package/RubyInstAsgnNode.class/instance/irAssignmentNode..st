*maglev-runtime
irAssignmentNode: srcIrNode
  |  node privSend sym stSym |
 stSym := stName .
 sym := name .
 (RubyCompilerState current lexLevelOkForFixedIvAccess) ifTrue:[ 
   (self instvarAccessKindFor: (stSym ifNil:[ sym ])  ) == 0 ifTrue:[
     ^ super irAssignmentNode: srcIrNode  "fixed or dynamic iv bytecode "
   ].
 ].
 stSym ifNil:[ stSym := (sym copyFrom: 2 to: sym size) asSymbol ].
 (privSend := GsComSendNode new)
   rcvr:  GsComVariableNode newSelf ;
   stSelector:  #rubyPrivateSize .
 (node := GsComSendNode new)
     rcvr:  GsComVariableNode newSelf ;
     stSelector:  #_rubyInstvarAt:put:privateSize: ;
     appendArgument:  ( GsComLiteralNode newObject: { stSym . sym . nil . 0 } );
     appendArgument:  srcIrNode ;
     appendArgument:  (self ir: privSend ) .
 ^ self ir: node

