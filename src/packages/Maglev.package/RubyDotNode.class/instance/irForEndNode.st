*maglev-runtime
irForEndNode
   | en |
   isInline ifFalse:[ self error:'inconsistent dot3 inline'].
   en := endNode irNode .
   endTmp ifNotNil:[ 
      en := GsComAssignmentNode _basicNew dest:(endTmp leaf) source: en .
      self ir: en .
   ].
   ^ exclusive 
        ifFalse: [ en]
        ifTrue:[ | send |
            (send := GsComSendNode new)
                rcvr:  en ;
                stSelector:  #-  ;
                appendArgument:
                    (GsComLiteralNode new leaf: (GsComLitLeaf new integerLiteral: 1)).
            self ir: send .
            send
        ]

