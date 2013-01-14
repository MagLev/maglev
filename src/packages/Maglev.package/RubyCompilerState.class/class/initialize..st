*maglev-runtime
initialize: environmentId
 "Note, we currently don't support fully general mixed-env ruby execution ; to do so 
  would require a stack of envId's "
 | clientData st  |
 clientData := GsProcess _current _newRubyThreadData . 
 st := clientData at: 1 "inline  self current" .
 st ifNil:[  st := self initializeForNewThread: environmentId ].
 st initialize: environmentId .
  ^ st

