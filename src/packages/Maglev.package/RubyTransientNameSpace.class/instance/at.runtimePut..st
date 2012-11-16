as yet unclassified
at: aSymbol runtimePut: aValue 
  | assoc pns |
super at: aSymbol runtimePut: aValue .

((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
  pns at: aSymbol runtimePut: aValue 
].
^ aValue

