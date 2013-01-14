(as yet unclassified)
walkWithScope: aScope
  " self receiverNode == nil ;  self argNodes == #()"
  | loopStk |
  loopStk := RubyCompilerState current loopStack .
  loopStk push: self .
  [ 
    conditionNode walkWithScope: aScope . 
    bodyNode walkWithScope: aScope .
  ] ensure:[
     loopStk pop: self
  ].
  blksInline :=  hasBeginOrRescue == nil

