*maglev-runtime
irForNode
    | node bg en   |
    isInline ifFalse: [ ^ super irForNode].
    bg := beginNode irNode .
    beginTmp ifNotNil:[ 
      bg := GsComAssignmentNode _basicNew dest:(beginTmp leaf) source: bg .
      self ir: bg .
    ].
     (node := GsComSendNode new)
            rcvr: bg ;
            stSelector:  #'to:do:' ;
            appendArgument: self irForEndNode ;
            optimize .
     self ir: node .
     ^ node

