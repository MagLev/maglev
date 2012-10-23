*maglev-runtime
_leafForVarAssoc: varInfo at: anOffset
   | aLeaf nA  |
   (aLeaf := varInfo leaf) ifNil:[  | nam |
      nam := varInfo key .
      varInfo kind == #blockArg ifTrue:[
        aLeaf := self newArgLeafNamed: nam number: -1  
     ] ifFalse:[
        anOffset <= (nA := self numArgs) ifTrue:[
          aLeaf := self newArgLeafNamed: nam number: anOffset 
        ] ifFalse: [
           aLeaf := self  newTempLeafNamed: nam  .
        ].
     ].
     varInfo leaf: aLeaf .
  ].
  ^ aLeaf

