*maglev-runtime
_startRuby: envId
      "ruby_selector_suffix dependent"

 | blockCopy argsCopy |
 blockCopy := block.
 argsCopy := args.
 RubyCompilerState initializeForNewThread: envId .
 ^ [
     RubyCompiler withRubyHandlers: envId main: false do: [
      envId == 1 ifTrue:[ blockCopy with: argsCopy perform:#'call#0*_' env: 1 ]
         ifFalse:[ Exception signal:'envId > 1 not supported' . nil ]
     ]
   ] onException: {  TerminateProcess . AbstractException } 
      do: { [:ex |
            GsProcess _current _handleTerminateProcess: ex .
            ex return: ex  "return exception as thread result"
          ] .
          [:ex |  ex return: ex  "return exception as thread result" ]
        }

