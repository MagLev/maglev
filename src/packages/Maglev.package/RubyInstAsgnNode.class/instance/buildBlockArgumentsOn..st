*maglev-runtime
buildBlockArgumentsOn: irBlock

  | aLeaf argLeaf n asgnNod |
  aLeaf := self irLeaf .
  n := irBlock numArgs + 1 .
  (argLeaf := GsComVarLeaf new)
       blockArg: aLeaf varName argNumber: n forBlock: irBlock.
  irBlock appendArg: argLeaf .
  asgnNod := (GsComAssignmentNode _basicNew
      dest: aLeaf
      source: ( GsComVariableNode new leaf: argLeaf ) ).
  irBlock appendStatement: asgnNod

