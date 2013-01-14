*maglev-runtime
initializeForNewThread: environmentId
 "Note, we currently don't support fully general mixed-env ruby execution ; to do so 
  would require a stack of envId's "
 | clientData  st  |
 clientData := GsProcess _current _newRubyThreadData .
 st := clientData at: 1 "inline  self current" .
 st ifNil:[
   st := self _basicNew initializeForNewThread: environmentId .
   clientData at: 1 put: st ;  "inline _rubyThreadDataAt:put:"
              "at:2 , last child process status, left as nil"
              at: 3 put: RubyCompilerStack new ; "instance_eval self's stack"
              at: 4 put: IdentitySet new ; "recursionGuardSet"
              at: 5 put: st rtModuleStack ;
              at: 6 put: RubyCompilerStack new . "evalArgs stack"
 ] ifNotNil:[
   st envId: environmentId .
   st class == self ifFalse:[  self error:'invalid RubyCompilerState'  ].
 ].
  ^ st

