(as yet unclassified)
walkWithScope: aScope
  | loop |
  loop := RubyCompilerState current topLoop .
  loop ifNotNil:[  loop setHasBreakNext ].
  walked ifNil:[
     valueNode walkWithScope: aScope
  ]

