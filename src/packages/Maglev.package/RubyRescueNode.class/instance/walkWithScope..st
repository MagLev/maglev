(as yet unclassified)
walkWithScope: aScope
   " argNodes inherits from RubyAbstractCallNode and is empty"
  | loop |
  loop := RubyCompilerState current topLoop .
  loop ifNotNil:[  loop setHasBeginRescue  ].
  bodyNode walkWithScope: aScope . 
  elseNode walkWithScope: aScope .
  rescueBodyNode walkWithScope: aScope

