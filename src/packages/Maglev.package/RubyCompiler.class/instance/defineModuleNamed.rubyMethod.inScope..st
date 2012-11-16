*maglev-runtime
defineModuleNamed: aSymbol rubyMethod: aNode inScope: parentArg 
  "called from generated code"
^ self defineModuleNamed: aSymbol rubyMethod: aNode inScope: parentArg 
    env: 1"__callerEnvId"

