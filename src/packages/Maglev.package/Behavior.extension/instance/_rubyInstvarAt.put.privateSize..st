*maglev-runtime
_rubyInstvarAt: descrArray put: aValue privateSize: privateSize  
  "called from generated code.
   descrArray is { stSymbol . rubySymbol . cachedClass . cachedOffset }
   Returns aValue "

  ^ self rubyClassInstVarNamed: (descrArray at:2)  put: aValue env: 1"__callerEnvId"

