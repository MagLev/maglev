*maglev-runtime
irNode
 |  node  sym stSym |
 stSym := stName . 
 sym := name .
 RubyCompilerState current lexLevelOkForFixedIvAccess ifTrue:[
   (self instvarAccessKindFor: (stSym ifNil:[ sym ]) ) == 0 ifTrue:[
       ^ super irNode  "fixed or dynamic iv bytecode "
   ].
 ].
 "block in non-bootstrap code uses this path, since eval can change self of a block"
 stSym ifNil:[ stSym := (sym copyFrom: 2 to: sym size) asSymbol ].
 (node := GsComSendNode new)
    rcvr:  GsComVariableNode newSelf ;
    stSelector:  #_rubyInstvarAt:  ;
    appendArgument:  ( GsComLiteralNode newObject: { stSym . sym . nil . 0 } ).
 ^ self ir: node

