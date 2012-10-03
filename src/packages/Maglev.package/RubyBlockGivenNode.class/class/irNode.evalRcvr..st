*maglev-runtime
irNode: theSelf evalRcvr: evalReceiver
  | node rcv  |
  evalReceiver ifNotNil:[  
     (rcv := GsComSendNode new)
       rcvr: (GsComLiteralNode newObject: GsProcess);
       stSelector: #_rubyEvalBlockArg .
      theSelf ir: rcv .
   ] ifNil:[  
     rcv := RubyCompilerState current topScope implicitBlockNotNil .
     rcv ifNil:[ ^ GsComLiteralNode newFalse ].
  ].
  (node := GsComSendNode new)
       rcvr:  rcv ;
       stSelector: #'~~'  ;
      appendArgument:   GsComLiteralNode newNil .
  ^ theSelf ir: node 

