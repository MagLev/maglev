*maglev-runtime
irOptArgNode
   | irCmp irArgVal astDefault leafLoc |
   leafLoc := location leaf .
   (irCmp := GsComSendNode new)
        rcvr: (GsComVariableNode new leaf: leafLoc );
        stSelector: #'=='  ;
        appendArgument: (GsComLiteralNode new 
          leaf: (GsComLitLeaf new specialLiteral:  UndefinedObject noArgNil )) .

    astDefault := self astDefaultVal .
    (irArgVal := GsComSendNode new)
        rcvr: irCmp ;
        stSelector: #ifTrue:ifFalse: ;
        appendArgument: (self newInlineBlock: [:block | 
             "for an arg like a=b=c=5   all vars get the default"
	    block appendStatement:( self irNodeWithDefault: astDefault irLocalAsgnValue) .  
            block ]) ;
        appendArgument: (self newInlineBlock: [:block | 
              "for a=b=c=5  ; a gets non-default pass value ; b,c get nil "
             self irNodeNonDefault: ( GsComVariableNode new leaf: leafLoc) block: block.
             block]);
        optimize .    "to make the ifTrue: inline"
    ^ irArgVal

