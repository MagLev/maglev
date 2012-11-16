*maglev-ast
buildBlockArgumentsOn: irBlock

  self _buildBlockArgumentsOn: irBlock .
  leftList size == 1 ifTrue:[  irBlock appendMasgnDummyArg "fix Trac 570"].

