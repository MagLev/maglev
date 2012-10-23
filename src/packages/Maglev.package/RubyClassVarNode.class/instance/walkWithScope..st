*maglev-runtime
walkWithScope: aScope 
  | cst |
  inMethod := RubyCompilerState current  topMethodDef isMethodDefinition .
  ^ super walkWithScope: aScope 

