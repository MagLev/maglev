*maglev-runtime
walkWithScope: aScope
  inMethod := RubyCompilerState current topMethodDef isMethodDefinition .
  super walkWithScope: aScope 

