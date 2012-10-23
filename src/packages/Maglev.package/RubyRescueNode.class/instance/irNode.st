*maglev-runtime
irNode
   | onArg doarg  nextRb blk send |
   onArg := rescueBodyNode irExceptionNode . 
   doarg := rescueBodyNode irBodyNode .
   blk := self newBlock:[:block |
     bodyNode irNodeListInto: block .
     block 
   ].
   elseNode ifNotNil:[ | elseBlk |
     elseBlk := self newBlock:[:block |
       elseNode irNodeListInto: block .
       block  
     ].
     (send := GsComSendNode new) 
        rcvr: blk ;
        stSelector: (RubyCompilerState current envId == 2 
                       ifTrue:[ #rescue2:do:else: ] ifFalse:[ #rescue1:do:else: ])  ;
        appendArgument:  onArg ;
        appendArgument:  doarg ;
        appendArgument:  elseBlk .
   ] ifNil:[
     (send := GsComSendNode new) 
        rcvr: blk ;
        stSelector: (RubyCompilerState current envId == 2 
                       ifTrue:[ #rescue2:do: ] ifFalse:[ #rescue1:do: ])  ;
        appendArgument:  onArg ;
        appendArgument:  doarg .
   ].
   self ir: send .
   ^ send

