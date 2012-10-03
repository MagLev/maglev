*maglev-runtime
irAssignmentNode: srcIrNode
  | holder snd |
  (holder := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: GsProcess"Ruby ::Thread") .
  snd := GsComSendNode new .
  vcOfs ifNotNil:[ :ofs |
    holder  stSelector: #_rubyEvalVc .
    snd rcvr: holder ;
      stSelector:  #at:put: ;
       appendArgument: (GsComLiteralNode newInteger: ofs ) .
  ] ifNil:[
    holder stSelector:  #_rubyEvalBinding .
    snd  rcvr: holder ;
      stSelector: #_tempAt:put: ;
      appendArgument: (GsComLiteralNode newObject: tmpName) .
  ].
  snd appendArgument: srcIrNode .
  ^ snd

