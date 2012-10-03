*maglev-runtime
walkWithScope: aScope
  | mth scp |
  mth := RubyCompilerState current topMethodDef .
  scp := mth scope .
  mthScope := scp .
  (location := scp locationForName: name ) ifNil:[
     self error:'VcGlobal not found' 
  ].
  super walkWithScope: aScope 

