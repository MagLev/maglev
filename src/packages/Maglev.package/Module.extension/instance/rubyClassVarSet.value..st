*maglev-runtime
rubyClassVarSet: aName value: aValue
  "a ruby primitive"

  ^ self addRubyClassVar: aName asSymbol value: aValue env: 1"__callerEnvId"

