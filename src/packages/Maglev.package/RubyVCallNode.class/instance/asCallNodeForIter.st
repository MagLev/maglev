as yet unclassified
asCallNodeForIter
  | node |
  (node := RubyCallNode _basicNew)
     position: position ;
     methodName: callName ;
     receiverNode: rcvrNode .
  ^ node 

