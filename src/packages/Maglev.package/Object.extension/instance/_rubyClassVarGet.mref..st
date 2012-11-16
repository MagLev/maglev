*maglev-runtime
_rubyClassVarGet: aSymbol  mref: modulRef 
  "called from generated code"
  ^ modulRef _classForRubyClassVar _rubyClassVarGet: aSymbol env: 1"__callerEnvId"

