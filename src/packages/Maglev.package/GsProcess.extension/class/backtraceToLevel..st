*maglev-runtime
backtraceToLevel: aLevel
| backtrace aFrame meth level |        
backtrace := Array new .
level := 1 .
[ level <= aLevel _and:[ (aFrame := self _frameContentsAt: level + 1) ~~ nil] ] whileTrue:[
    level := level + 1.
    meth := aFrame at: 1 . "a GsNMethod"
    meth == nil ifFalse:[ | farr env |
      env := meth environmentId .
       farr := { 
         meth _descrForStack . 
          (meth _lineNumberForStep: (meth _previousStepPointForIp: (aFrame at: 2)))  + meth _lineNumberBias  . 
         env . 
         meth homeMethod _rubyName 
       }.
       env ~~ 0 ifTrue:[ | fileLine |
          farr add: meth isRubyBridgeMethod .
         fileLine := meth _fileAndLine .
         fileLine ifNotNil:[ farr addAll: fileLine ].
       ].
       backtrace add: farr .
    ].
].
^ backtrace

