*maglev-runtime
rubyFullName: envId
  "called from Smalltalk code only"
  | ns nam |
  (ns := self nameSpace: envId) ifNotNil:[ ^ ns fullName ].
  ((nam := name) isNil or: [nam size == 0]) ifTrue: [
    self isMeta ifTrue: [ ^ (destClass rubyFullName: envId), ':Class' ].
    ^ '#<Class:0x', self asOop hex, '>' ].
  ^ String withAll: nam

