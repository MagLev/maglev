*maglev-runtime
close
 | envId |
 envId := 1"__callerEnvId" .
 forWrite ifTrue:[ 
   self _flush: nil env: envId .
   self _streamOp: 1 with: nil . "close underlying stream"
 ] ifFalse:[ 
   self _finishReading
 ]

