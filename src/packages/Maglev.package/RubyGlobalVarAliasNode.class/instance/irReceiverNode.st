*maglev-runtime
irReceiverNode
  | snd| 
  (snd := GsComSendNode new)
     rcvr: (GsComLiteralNode newObject: Object) ;
     stSelector: #nameSpaceForGlobalVar .
  ^ snd

