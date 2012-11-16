*maglev-runtime
irArgNodes
      "return array of IR nodes for the arguments"
    | lst sz res  |
    lst := argsList . "was computed during walkWithScope: phase "
    lst ifNil:[ self error:'AbstractCallNode.irArgNodes - missed walk args']. 
    res := Array new:(sz := lst size) .
    1 to: sz  do:[:n |  res at: n put: ( lst at: n) irArgNode  ].
    ^ res

