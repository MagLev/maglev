*maglev-runtime
addRubyClassVar: aSymbol  value: aValue  mref: modulRef 
  "called from generated code"
   ^ modulRef _classForRubyClassVar  addRubyClassVar: aSymbol value: aValue env: 1"__callerEnvId"

