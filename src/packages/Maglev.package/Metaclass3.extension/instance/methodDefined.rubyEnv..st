*maglev-runtime
methodDefined: aSymbol rubyEnv: envId
  "does not consider the package policy.
   Returns nil or an Array { cm . protectionOverride }."
  ^ self lookupSelector: aSymbol rubyEnv: envId

