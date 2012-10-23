*maglev-runtime
irExceptionNode
  | node exList eNodes | 
  (eNodes := exceptionNodes)  ifNil:[ ^ defaultNode irNode].  
  exList := eNodes list . 
  exList size == 0 ifTrue:[   
     eNodes isSplatNode  ifTrue:[
       node := eNodes irNode   " rescue  *aList  " 
     ] ifFalse:[
       self error:'unrecognized argument to "rescue" ' .
     ].
  ] ifFalse:[
      node := ( exList at: 1 ) irNode  .
      2 to: exList size do:[ :n | | ea | 
        ea := exList at: n .
        node := GsComSendNode new rcvr: node ;
            stSelector: #addException:  ;
            appendArgument: ea irNode  .
      self ir: node .
      ].
  ].
  ^ node

