*maglev-runtime
buildBlockArgumentsOn: irBlock
  self _buildBlockArgumentsOn: irBlock .
  irBlock setLastArgStar

