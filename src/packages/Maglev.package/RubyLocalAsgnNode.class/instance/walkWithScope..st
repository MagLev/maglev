*maglev-runtime
walkWithScope: aScope
  | loc | 
  isBlockArg ifTrue:[ 
     loc := aScope locationForBlockArg: name . 
     createsVar := false .
   ] ifFalse:[  | nm |
     loc := aScope locationForExistingName: (nm := name) depth: 0 . 
     loc ifNil:[ 
       loc := aScope newVarLocation: nm . 
       createsVar := true .
     ] ifNotNil:[
       createsVar := false .
     ].
  ].
  location := loc .
  super walkWithScope: aScope

