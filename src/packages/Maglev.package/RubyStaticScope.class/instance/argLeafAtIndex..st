*maglev-runtime
argLeafAtIndex: aNumber
  | nA aLeaf |
  aNumber <= (nA := self numArgs) ifTrue: [
    aLeaf := self _argLeafAtOffset: aNumber .
    aLeaf isArg ifFalse:[ self error:'not an arg leaf'].
  ] ifFalse: [
    self error:'invalid use of argLeafAtIndex:  '.
  ].
  ^ aLeaf

