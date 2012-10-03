*maglev-runtime
handleRubyThrow: anException sysExitBlock: sysExitBlk main: mainThrBool 
  | namErr sigNum throwExBlk |
  throwExBlk := [ :ex|  | resigEx | 
     mainThrBool ifTrue:[  
        (resigEx := NameError new) messageText:'no catch found for throw';
                                        name: ex name .
     ] ifFalse:[
        (resigEx := ThreadError new) messageText:'no catch found for throw ,', ex name.
     ].
     ex resignalAs: resigEx
   ].
  (sigNum := anException signalNumber) ifNotNil:[ | envId tns dict blk |
    envId := 1 "assume env 1" .
    tns := RubySignal transientNameSpaceForStore: envId .
    dict := tns at:#TrappedSignals . 
    (blk := dict at: sigNum ) ifNotNil:[ | handledOk |
      sigNum == (tns at: #SigTERM) ifTrue:[
        [ | exc |
          (Delay forMilliseconds: 5000 ) highPriorityWait . 
           GsFile gciLogServer:'SIGTERM , 5 second delay expired' .
           Error signalFatalSigTerm .
        ] forkAt: 40"inline highestPriority" with: #() env: 1 .
        ProcessorScheduler scheduler yield .
      ].
      [
        blk @ruby1:__signal_callback .
        handledOk := true
      ] onException: { RubyThrowException . RubySystemExit } 
        do: {  throwExBlk . sysExitBlk }.
      handledOk ifNotNil:[
        anException resume  "resume execution where signal was received"
      ].
      anException outer
    ] ifNil:[ 
      Break new 
         details: 'no Ruby handler for signal ', sigNum asString ;
         signalNotTrappable .
      anException resume .  "in case execution continued from debugger"
    ]
  ] ifNil:[ 
    throwExBlk value: anException 
  ].

