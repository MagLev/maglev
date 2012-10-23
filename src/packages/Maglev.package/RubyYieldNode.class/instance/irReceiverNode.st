*maglev-runtime
irReceiverNode
  evalRcvr ifNotNil:[ | snd |
    (snd := GsComSendNode new)
       rcvr: (GsComLiteralNode newObject: GsProcess);
       stSelector: #_rubyEvalBlockArg .
    self ir: snd .
    ^ snd
  ] ifNil:[
    ^  self currentScope implicitBlockVar
  ].

