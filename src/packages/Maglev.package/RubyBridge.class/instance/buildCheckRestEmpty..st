*maglev-runtime
buildCheckRestEmpty: irMethNode
  | sizNode tstNode ifNode blkNode   |
  (sizNode := GsComSendNode new) 
     rcvr:  self restNode ;
     stSelector: #size .
  ( tstNode := GsComSendNode new )
     rcvr: sizNode ;
     stSelector:  #==  ;
     appendArgument: (GsComLiteralNode newInteger: 0) .
  ( blkNode := GsComBlockNode new) 
     lexLevel: 1 ;
     appendStatement: ( self irArgumentErrorNode:  true"too many" ) .
  ( ifNode := GsComSendNode new )
     rcvr: tstNode ;
     stSelector:  #ifFalse:  ;
     appendArgument: blkNode ;
     optimize .

  irMethNode appendStatement: ifNode .

