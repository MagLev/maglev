*maglev-runtime
blockArgLeaf: aVarLeaf
  | leaf |
  (leaf := aVarLeaf) ifNil:[
     ( leaf := incBlockArgLeaf) ifNil:[ 
       leaf := GsComVarLeaf new methodArg: self implicitBlockTempName argNumber: self numArgs + 1 .
     ].
  ].
  incBlockArgLeaf := leaf .
  ^ leaf 

