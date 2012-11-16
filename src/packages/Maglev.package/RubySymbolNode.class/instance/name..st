*maglev-ast
name: aSymbol 
  aSymbol _isSymbol ifFalse:[ RubyParserM signalError:'RubySymbolNode value not a Symbol'].
  name := aSymbol 

