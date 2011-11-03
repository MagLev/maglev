
set class RubyScopeIncomingBlockVar
category: '*maglev-runtime'
method:
key: nameSym kind: kindSym ofs: anOffset
  readCount := 0  .
  ^ super key: nameSym kind: kindSym ofs: anOffset

%


set class RubyScopeIncomingBlockVar
category: '*maglev-runtime'
method:
readCount
  ^ readCount

%


set class RubyScopeIncomingBlockVar
category: '*maglev-runtime'
method:
readCount: delta
  readCount := readCount + delta  .
  ^ isBlockInt "receiver represents an incoming block arg, if not bootstrap"

%


set class RubyScopeIncomingBlockVar
category: '*maglev-runtime'
method:
toProcInfo
  ^ toProcInfo 

%


set class RubyScopeIncomingBlockVar
category: '*maglev-runtime'
method:
toProcInfo: aVarInfo
  toProcInfo := aVarInfo

%


set class RubyScopeIncomingBlockVar
category: '*maglev-runtime'
method:
toProcInfo: aVarInfo boot: inBoot
  toProcInfo := aVarInfo .
  isBlockInt :=  inBoot ifTrue:[ 0 ] ifFalse:[ 1 ].

%

