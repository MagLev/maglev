*maglev-runtime
irC2Node: aSelectorPair
  " selector pair is  #( fixed selector, dynamic resolve selector ) "
  | node cRef   |
  cRef := self buildConstRef .  
  cRef class == Array ifTrue:[ | arr lhsNode ofs |
    arr := cRef .
    cRef := RubyConstantRef new "initialize sets lexPathSize to zero" .
    lhsNode := arr at: 1 .
    cRef addAll: arr excludingFirst: 1 . "all path terms after first :: "
    ( node := GsComSendNode new)
       stSelector: ( aSelectorPair at: 2) ;  "dyn_resolveInContext: or dyn_definedQinContext:"
       rcvr: (GsComLiteralNode newConstantRef: cRef ) ;
       appendArgument: lhsNode .  
  ] ifFalse:[ 
    dynamicTypeError ifTrue:[ cRef setDynamicTypeError ].
    (node := GsComSendNode new)
       stSelector:  ( aSelectorPair at: 1)  ;  " resolveConst or definedQconst "
       rcvr: (GsComLiteralNode newConstantRef: cRef ) .
  ].
  self ir: node .
  ^ node

