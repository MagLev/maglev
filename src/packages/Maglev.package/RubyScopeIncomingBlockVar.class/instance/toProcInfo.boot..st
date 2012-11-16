*maglev-runtime
toProcInfo: aVarInfo boot: inBoot
  toProcInfo := aVarInfo .
  isBlockInt :=  inBoot ifTrue:[ 0 ] ifFalse:[ 1 ].

