as yet unclassified
at: aSymbol classVarPut: aValue 
  |  pns |
super at: aSymbol classVarPut: aValue .

((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
  pns at: aSymbol classVarPut: aValue 
].
^ aValue


