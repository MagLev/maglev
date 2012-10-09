*maglev-runtime
_classFor: aSymbol inModule: aModule env: envId
  self _assocFor: aSymbol inClass: aModule env: envId error: false .
  "caller must re-resolve"
  ^ nil

