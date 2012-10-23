*maglev-runtime
irNodeWithDefault: irRhs
  "Use the specified irRhs instead of the right-most value in ruby   a=b=c=5"
  | val |
  val := valueNode.
  val class == RubyLocalAsgnNode ifTrue:[ 
     ^ self irAssignmentNode: (val irNodeWithDefault: irRhs)
  ].
  ^ self irAssignmentNode: irRhs 

