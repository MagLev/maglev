*maglev-runtime
_rubyInstvarAt: descrArray
 "A ruby primitive, and called from generated code"
  "descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset } "
  ^ self rubyClassInstVarNamed: (descrArray at: 2  "rubyName" ) env: 1"__callerEnvId"

