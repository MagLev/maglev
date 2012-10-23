*maglev-runtime
rubyConstDecl: aSymbol put: aValue
   "called from generated code"
  ^ self virtualClass rubyConstAt: aSymbol env: 1"__callerEnvId" put: aValue .

