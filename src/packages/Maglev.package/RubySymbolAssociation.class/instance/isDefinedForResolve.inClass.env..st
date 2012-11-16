*maglev-runtime
isDefinedForResolve: aSymbol inClass: aClass env: envId 
  "returns self or nil "
  isDefined ifNotNil:[ ^ self ].
  ^ nil

