as yet unclassified
asCallNodeForIter
  | node |
  (node := RubyCallEvalNode _basicNew)
     position: position ;
     methodName: callName ;
     receiverNode: rcvrNode .
  ^ node

