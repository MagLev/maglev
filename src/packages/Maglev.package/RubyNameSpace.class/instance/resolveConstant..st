*maglev-runtime
resolveConstant: aSymbol
  "runtime constant resolution, returns a RubySymbolAssociation or nil"
  ^ self _at: aSymbol otherwise: nil . 

