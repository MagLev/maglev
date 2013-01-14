*maglev-runtime
_inspect
  | dots |
  dots := isDot3 ifTrue:[ '...' ] ifFalse:[ '..' ].
  ^ '[:flip, ', firstNode _inspect, ', ', dots ,  ' , ', secondNode _inspect , $]

