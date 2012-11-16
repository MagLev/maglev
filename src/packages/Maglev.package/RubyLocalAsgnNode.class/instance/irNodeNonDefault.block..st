*maglev-runtime
irNodeNonDefault: irRhs block: irBlock
  "For a=b=c=5   a := non-default RHS , b:=nil, c:= nil"
  | val |
  val := valueNode.
  val class == RubyLocalAsgnNode ifTrue:[  "b=c=nil"
    irBlock appendStatement: (val irNodeWithDefault: GsComLiteralNode newNil)
  ].
  irBlock appendStatement: (self irAssignmentNode: irRhs) . "a := non-default"
  ^ nil

