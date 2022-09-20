*maglev-runtime
_rubyInspect: envId
  "called from smalltalk code"
   (self isMeta or: [self isRubySingletonClass or: [self isMetaModule]]) 
    ifTrue: [^ self _rubyInspectSingletonClass]
    ifFalse:[ |name|
      name := self rubyFullName: envId .
      (name isNil or: [name size == 0]) ifTrue: [
        ^ '#<Class:0x', self asOop hex, '>' ].
      ^ name]

