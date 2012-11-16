*maglev-runtime
evalDashEStrings: anArray
   |tns oldFile |
   oldFile := (tns := Object transientNameSpaceForStore: 1) at: #'__FILE__' otherwise: nil .
   [ |evalString comp|
       tns at: #'__FILE__' compilePut: '-e' .

       evalString := anArray inject: '' into: [:acc :el | acc, Character lf, el ] .
       RubyCompilerState initialize .
       comp := RubyCompiler new .
       comp evaluateString: evalString with: { nil . nil } withSelf: Object 
             binding: nil fileName: '-e' lineNumber: 1  env:1 .
       (SessionTemps current at: #MAGLEV_commitFlag otherwise: false) ifTrue: [ 
           self commitTransaction 
       ] .
   ] onException: Error do: [:e|
     tns at: #'__FILE__' compilePut: oldFile .
     e outer
   ] .
   tns at: #'__FILE__' compilePut: oldFile .
   ^ self .

