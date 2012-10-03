*maglev-runtime
requirePrimitives: envId
  ^ self  _requirePrimitives_traceGlobals: 0 traceLocals: 0 env: envId reload: false .

