*maglev-runtime
irArgNodes
  "replicate the args to the method containing the send of  super"
  | mth arr sel itr |
  mth := RubyCompilerState current topMethodDef . 
  sel := mth methodSelector .
  sel ifNil:[  self signalParseError:
       'super with implicit args supported only in a compile-time def '
  ].
  methSelector := sel .  
  arr := { } .
  mth comIrMethNode arguments do:[:aLeaf |
     arr add: (GsComVariableNode new leaf: aLeaf )
  ].
  (itr := iterNode) ifNotNil:[
    "override implicit block arg with the block passed to super"
    arr at: arr size put: itr irNode
  ].
   ^ arr .

