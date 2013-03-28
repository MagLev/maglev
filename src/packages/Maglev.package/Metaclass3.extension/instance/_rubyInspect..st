*maglev-runtime
_rubyInspect: envId
  "called from smalltalk code"
   (self isMeta or: [self isRubySingletonClass]) 
    ifTrue: [^ self _rubyInspectSingletonClass]
    ifFalse:[ |name|
      name := self rubyFullName: envId .
      (name isNil or: [name size == 0]) ifTrue: [
        self isMeta ifTrue: [ ^ (destClass rubyFullName: envId), ':Class' ].
        ^ '#<Class:0x', self asOop hex, '>' ].
      ^ name]

