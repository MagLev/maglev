*maglev-runtime
s_forRp: condArg block: blkArg bool: cfBool
  | cls  node |
  cls := cfBool ifTrue:[  RubyWhileNode ] ifFalse:[ RubyUntilNode ].
  (node := cls _basicNew )
     conditionNode: condArg ;
     bodyNode: blkArg ;
     conditionFirst: cfBool ;
     baseSelector: (self selectorFor: cfBool ) .
  ^ node

