*maglev-runtime
setMagLevOptions: argsArray
  "Set options to the vm passed by the maglev-ruby script.

   TODO: I suppose we could store the blocks in a dictionary, and then
   just invoke them for each option passed on the command line..."
| tmps captStack |
tmps := SessionTemps current .
captStack := true .
1 to: argsArray size do:[:n | | ea |
  ea := argsArray at: n .
  ea == #persistent ifTrue:[ tmps at: #MAGLEV_persistentFlag put: true ] ifFalse:[
  ea == #commit ifTrue:[ tmps at: #MAGLEV_commitFlag put: true ;
                              at: #MAGLEV_persistentFlag put: true ] ifFalse:[
  ea == #traceLoad ifTrue:[ tmps at: #MAGLEV_RubyFile_traceLoad put: true ] ifFalse:[
  ea == #logSexp ifTrue:[ tmps at: #MAGLEV_logSexp put: true ] ifFalse:[
  ea == #traceGlobals ifTrue:[ RubyNameSpace traceGlobals: 2 ] ifFalse:[
  ea == #noCaptureStack ifTrue:[ captStack := false ]          
  ]]]]]
] .
System gemConfigurationAt:#GemExceptionSignalCapturesStack put: captStack

