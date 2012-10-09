*maglev-runtime
buildIrMethodNode: aBlock
  | cmState node |
  (cmState := RubyCompilerState current) pushMethodDef: self .
  (node := GsComMethNode newRuby)    
     environment: cmState envId ;
     setRubyLineNumberBias:  self lineBias ;
     fileName: self fileName source: self source .
  [
    aBlock value: node  .
  ] ensure:[
    cmState popMethodDef: self 
  ].
   self ir: node .
   ^ node

