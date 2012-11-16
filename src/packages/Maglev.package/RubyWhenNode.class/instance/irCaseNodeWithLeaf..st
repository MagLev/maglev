*maglev-runtime
irCaseNodeWithLeaf: aLeaf
  | send |
  (send := GsComSendNode new )
    rcvr: (self irCaseExpressionWithLeaf: aLeaf ) ;
    stSelector:  #ifTrue:ifFalse:   ;
    appendArgument: (bodyNode irBlockNodeInline: self) ;
    appendArgument:
      (self newInlineBlock:
	[:block |    "nextCase is the next WhenNode  within the CaseNode "
	  nextCase irCaseNodeListWithLeaf: aLeaf into: block .
          block] ) ;
        optimize .
    ^ send

