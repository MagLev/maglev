*maglev-runtime
_newLeaf: nam offsetInScope: anOffset kind: kind
  | aLeaf nA  |
  kind == #blockArg ifTrue:[
    aLeaf := self newArgLeafNamed: nam number: -1  
  ] ifFalse:[
    anOffset <= (nA := self numArgs) ifTrue:[
    aLeaf := self newArgLeafNamed: nam number: anOffset 
    ] ifFalse: [
      aLeaf := self  newTempLeafNamed: nam  .
    ].
  ].
  ^ aLeaf

