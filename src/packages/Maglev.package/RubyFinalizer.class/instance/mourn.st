*maglev-runtime
mourn
  | theProcs  theObj |
  "envId assumed 1" 
  theProcs := procs .
  theObj := obj .
  "do this iteration in Smalltalk to catch all possible exceptions"
  1 to: theProcs size do:[ :n | | aProc |
      aProc := theProcs at: n .
      [ 
         self @ruby1:__finalize: aProc _: theObj .
      ] onException: AbstractException do:[:ex | "ignore" ].
  ].
  self @ruby1:__finalize_done 

