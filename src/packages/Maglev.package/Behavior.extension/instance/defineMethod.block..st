*maglev-runtime
defineMethod: aSymbol block: blockArg
  "a ruby primitive"
  ^ self defineMethod: aSymbol block: blockArg env: 1"__callerEnvId"

