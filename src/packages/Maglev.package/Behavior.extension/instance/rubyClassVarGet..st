*maglev-runtime
rubyClassVarGet: aString
  "a ruby primitive "
  ^ self theNonMetaClass _rubyClassVarGet: aString asSymbol  env: 1"__callerEnvId"

