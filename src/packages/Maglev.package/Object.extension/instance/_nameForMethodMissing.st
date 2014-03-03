*maglev-runtime
_nameForMethodMissing
  | myName |
  "A ruby primitive , part of fix for Trac 752"
  myName := self virtualClass rubyFullName: 1"__callerEnvId".
  (myName == nil) 
    ifTrue: [^ 'Unknown']
    ifFalse: [^ myName]

