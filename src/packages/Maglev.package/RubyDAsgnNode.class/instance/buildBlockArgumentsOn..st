converting
buildBlockArgumentsOn: irNode

  isBlockArg ifFalse:[ self error:'inconsistent isBlockArg in Dasgn'].
  irNode appendArg:  self irLeaf

