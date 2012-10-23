*maglev-runtime
execMethodBridge: optArgsDescr env: envId
   | node cm | 
   (node := GsComMethNode newRuby)
     environment: envId ;
     selector: from  ; sourceOffset: 1 ;
     source: 
'<an execMethod  bridge method>
  ' .
  "inline buildMethodArgumentsOn:"
  node appendArg: (self argLeaf: 1 "ctx") ; appendArg:(self argLeaf: 2 "nonBrMeth") ;
     appendArg: self restLeaf ; appendArg: self blockLeaf .

  self buildExecBodyOn: node argsDescr: optArgsDescr .
  node class: RubyUnboundMeth .
  cm := RubyCompiler new  compiledMethodForIR: node .
  ^ cm

