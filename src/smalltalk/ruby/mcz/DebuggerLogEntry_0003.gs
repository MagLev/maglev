
set class DebuggerLogEntry
category: '*maglev-continuation creation'
classmethod:
rubyCreateContinuationLabeled: aString
"snap off a continuation at this point and stash it in the queue"
    |action cc logEntry |
    logEntry := (self error: aString).
    action := [:cont | cc := cont. #create ] callCC.
    action == #create ifTrue: [
      ^ (logEntry continuation: cc convertToPersistableState) addToLog; yourself].
    action == #debug ifTrue: [ | meth |
      meth := self class compiledMethodAt: #remoteBreakpointMethod.
      meth setBreakAtStepPoint: 1.
      self remoteBreakpointMethod ].
    (action == #stop and: [GsProcess _current == cc]) ifTrue: [ GsProcess _current suspend ].

%

