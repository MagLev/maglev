*maglev-runtime
defineMethod: aSymbol method: aMethod
    "a ruby primitive"
  ^ self defineMethod: aSymbol method: aMethod env: 1"__callerEnvId"

