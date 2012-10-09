*maglev-runtime
resolveGlobalVarValue
  "called from generated code.
   returns nil or the value of the global variable. "

| assoc |
assoc :=  self resolveGlobalVarAssoc .
assoc ifNil:[
  assoc := Object rubyGlobalVarAssoc: (self at:1) env: 1"__callerEnvId" .
  self setGlobalAssoc_noMarkDirty: assoc . "cache it"
].
^ assoc globalVarValue .

