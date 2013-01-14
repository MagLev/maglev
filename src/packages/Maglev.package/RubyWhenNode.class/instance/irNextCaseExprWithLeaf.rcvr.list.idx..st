*maglev-runtime
irNextCaseExprWithLeaf: aLeaf rcvr: aSend list: list idx: idx
  | nextSnd |  
  idx > list size ifTrue:[ ^ nil ].  

  (nextSnd := GsComSendNode new)
     rcvr: aSend ;
     stSelector:  #or:  ;
     appendArgument:( 
          self newInlineBlock:[ :blk | | eqeqeqSend nxt  |
            eqeqeqSend := (list at: idx) irWhenExpressionWithLeaf: aLeaf . 
            blk appendStatement: eqeqeqSend .
            nxt := self irNextCaseExprWithLeaf: aLeaf rcvr: eqeqeqSend list: list idx: idx + 1.
            nxt ifNotNil:[ blk appendStatement: nxt ].
            blk 
          ] ) .
   nextSnd optimize . 
   self ir: nextSnd .
   ^ nextSnd 

