*maglev-runtime
walkWithScope: aScope
  | newScop n bdy args mArgs  | 
  scope := (newScop := RubyBlockStaticScope new )
    enclosingScope: aScope ;
    requiredArgs: 0;
    restArg: -1 ;
    _nameSpace: aScope nameSpace  . 
  [ (args := varNode) ifNotNil: [
       varNode walkWithScope: newScop.
       (mArgs := multArgsNode) ifNotNil:[ mArgs walkWithScope: newScop ].
    ].
    (bdy := bodyNode) ifNotNil: [
        bdy walkWithScope: newScop.
     ].
  ] ensure: [
    newScop _nameSpace: nil "avoid commit of tns"
  ]

