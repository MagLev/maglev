*maglev-runtime
rubyClassInstVarNamed: aSymbol put: anObject env: envId
  | ns |
  ns := self transientNameSpaceForStore: envId .
  ^ ns at: aSymbol runtimePut: anObject

