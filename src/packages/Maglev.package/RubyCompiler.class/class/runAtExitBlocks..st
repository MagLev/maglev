*maglev-runtime
runAtExitBlocks: envId
  | tmps arr |
  arr := (tmps := SessionTemps current) at:#RubyExitHandlers otherwise: nil .
  arr ifNotNil:[  | sysExitBlk sz blk prevSiz count n |
    n := 0 .
    count := 0 .
    prevSiz := arr size .
    [ blk := arr _rubyDeleteAt: -1 . blk ~~ nil ] whileTrue:[
      n := n + 1 .
      [ [ [  
            blk @ruby1:value 
           ] onException:  RubyThrowException do: [ :ex|  
               self handleRubyThrow: ex sysExitBlock: sysExitBlk main: true . 
           ].
        ] onException: RubyBreakException do:[:ex |
           ex resignalAs: (CannotReturn new messageText:'break not within block nor while/for loop')
        ]
      ] onException: { RubySystemExit . AbstractException } do: 
       { 
          [:ex | ex outer  "defer to top level handlers for SystemExit " 
          ] .
          [ :ex |  | msg | 
           msg := [  'error , ' , ex asString .
           ]  onException: AbstractException do:[:exb | 
              ex return: 'error getting exception string' 
           ].
           GsFile gciLogClient:  'during at_exit handler ' , n asString , ': ' , msg . 
           (tmps at: #Maglev_ruby_debugFlag otherwise: false) ifTrue:[  
             self pause  "for debugging at_exit problems"
           ].
           ex return "return from onException:do: to try next exit handler"
          ] 
        } .
      sz := arr size .
      sz >= prevSiz ifTrue:[
        count := count + 1 .
        count > 10000 ifTrue:[ 
          Exception new details:'infinite loop in atexit processing' ;
             signalNotTrappable 
        ].
      ] ifFalse:[
        prevSiz := sz .
        count := 0 .
      ].
    ].
  ].
  ProcessorScheduler scheduler _killOtherRubyThreads

