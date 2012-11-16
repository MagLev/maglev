*maglev-runtime
withRubyHandlers: envId main: mainThrBool do: aBlock 
  | sysExitBlk |
  sysExitBlk := [ :ex|  | status nx mainThr currThr runAtExits runAtExitBlk |    
    mainThr := (SessionTemps current at:#RubyMainThread otherwise:nil).
    status := ex status  .
    runAtExits := ex runAtExitHandlers .
    runAtExitBlk := [ 
      [ RubyCompiler runAtExitBlocks: envId 
      ] on: RubySystemExit do: [ :sx |
        status := sx status . 
        sx return  "abandon further execution of at_exit blocks"
      ]
    ] .
    currThr := GsProcess _current .
    (mainThr ~~ currThr and:[ mainThr ~~ nil and:[ mainThr alive]]) ifTrue:[ | stat |
      stat := mainThr rubyStatus .
      (stat = 'sleep' or:[ stat = 'run']) ifTrue:[
        mainThr priority: Processor highestPriority . 
        (nx := RubySystemExit new) status: ex  status  ; args: { ex status } .
        mainThr signalException: nx .
        Processor yield.
      ].
      runAtExits ifTrue:[ runAtExitBlk value ].
      AbstractException signal: 'Kernel.exit invoked from non-main thread, and main thread not alive' .
    ] ifFalse:[
      runAtExits ifTrue:[ runAtExitBlk value ]
    ].
    status = 0 ifTrue:[  ex return: true].
    (nx := RubySystemExit new) status: status  ; args: { status } .
    nx signal "return error to topaz"
  ].
 ^ [ 
      [ | currThr nx res | 
        res := aBlock value .
        (SessionTemps current at:#RubyMainThread otherwise:nil) == GsProcess _current 
		ifTrue:[ (RubySystemExit new status: 0) signal ].
        res
      ] onException: { RubyThrowException . RubySystemExit } 
        do: { [ :ex|  self handleRubyThrow: ex sysExitBlock: sysExitBlk main: mainThrBool ]  . 
               sysExitBlk }.
   ] onException: AbstractException do:[:ex || stmps |
     stmps := SessionTemps current.
     (ex isKindOf: RubyBreakException) ifTrue:[
       "Note,  onException:{ exA . exB} do:{ }  form of handler 
        not recognized by signalRubyBreak bytecode."
       ex resignalAs: (CannotReturn new messageText:'break not within block nor while/for loop')
     ].
     ((stmps at:#RubyExitOnException otherwise: false) or: [(GsProcess _current threadDataAt: #RubyExitOnException) == true]) ifTrue:[ 
       (stmps  at:#Maglev_ruby_debugFlag otherwise: false) ifTrue:[
          GsFile gciLogServer:'an Exception would cause exit when abort_on_exception is true'.
          self pause .  "stop for debugging the application"
       ].
       ex resignalAs:( RubySystemExit new status: 0) .
       AbstractException signal:'should not be here after abort_on_exception activated'.
     ] ifFalse:[
       ex outer
     ]
   ]

