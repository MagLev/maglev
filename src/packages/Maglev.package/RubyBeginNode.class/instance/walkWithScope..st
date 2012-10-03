(as yet unclassified)
walkWithScope: aScope
  | loop |
  loop := RubyCompilerState current topLoop .
  loop ifNotNil:[  loop setHasBeginRescue ].
  bodyNode walkWithScope: aScope

