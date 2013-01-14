*maglev-runtime
rubyConstAt: aName put: aValue
  "called from generated code, for a ConstDeclNode."
  ^ self rubyConstAt: aName env: 1"__callerEnvId" put: aValue

