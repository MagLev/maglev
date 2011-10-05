
set class RubyBlockGivenNode class
category: '*maglev-runtime'
method:
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

%


set class RubyBlockGivenNode
category: '*maglev-runtime'
method:
irNode
  ^ RubyBlockGivenNode irNode: self evalRcvr: evalRcvr

%


set class RubyBlockGivenNode
category: '*maglev-runtime'
method:
walkWithScope: aScope
  RubyCompilerState current topMethodDef setHasBlockArgRef ifTrue:[
    "inEval" evalRcvr := true .
  ].
  super walkWithScope: aScope 

%

