*maglev-runtime
_initialize
  "executed during env 1 bootstrap"
  | dict | 
  dict := SymbolKeyValueDictionary new .
 { { #'$!' . RubyGlobalLastException . RubyGlobalNotAssignable } . 
    { #'$@' . RubyGlobalLastExcBackTrace . RubyGlobalLastExcBackTraceAsgn } . 
    { #'$~' . RubyVcGlobalNode . RubyVcGlobalLastMatchAsgn } . 
    { #'$_' . RubyVcGlobalNode . RubyVcGlobalAsgNode } . 
    { #'$?' . RubyGlobalProcessState . RubyGlobalNotAssignable } . 
    { #'$stdout' . RubyGlobalVarNode . RubyGlobalAsgnStdoutNode } . 
    { #'$stderr' . RubyGlobalVarNode . RubyGlobalAsgnStdoutNode } . 
    { #'$stdin' . RubyGlobalVarNode . RubyGlobalAsgnStdinNode   } }
  do:[:arr|
    dict at:(arr at:1) put: arr
  ].
  SpecialGlobalNodesDict := dict .

