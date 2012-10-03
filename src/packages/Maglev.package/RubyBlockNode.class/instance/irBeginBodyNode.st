*maglev-runtime
irBeginBodyNode
  "receiver is body of a begin..end that contains no rescue or ensure"
  | node |
  (node := GsComSendNode new)
     rcvr:  (self irBlockNode: self)  ;
     stSelector: #value  .
  self ir: node .
  ^ node

