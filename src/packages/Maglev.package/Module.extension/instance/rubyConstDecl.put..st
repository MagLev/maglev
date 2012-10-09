*maglev-runtime
rubyConstDecl: aSymbol put: aValue
   "a ruby primitive"
  ^ self rubyConstAt: aSymbol env: 1"__callerEnvId" put: aValue .

