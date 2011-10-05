
set class DebuggerLogEntry class
category: '*maglev-continuation creation'
method:
rubyCreateContinuationLabeled: aString
"snap off a continuation at this point and stash it in the queue"
    |action continuation logEntry |
    logEntry := (self error: aString).
    action := [:cont | continuation := cont. #create ] callCC.
    action == #create ifTrue: [
      ^ (logEntry continuation: continuation convertToPersistableState) addToLog; yourself].
    action == #debug ifTrue: [ | meth |
      meth := self class compiledMethodAt: #remoteBreakpointMethod.
      meth setBreakAtStepPoint: 1.
      self remoteBreakpointMethod ].
    (action == #stop and: [GsProcess _current == continuation]) ifTrue: [ GsProcess _current suspend ].

%

