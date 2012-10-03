*maglev-runtime
_rubyClassVarGetOrNil: aSymbol  mref: modulRef 
  "called from generated code"
  ^ modulRef _rubyClassVarGetOrNil: aSymbol env: 1"__callerEnvId"

