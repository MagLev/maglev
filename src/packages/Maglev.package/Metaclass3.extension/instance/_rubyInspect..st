*maglev-runtime
_rubyInspect: envId
  "called from smalltalk code"
   self isMeta ifTrue:[  | res |
     res := '#<Class:' copy .
     res add: (destClass rubyFullName: envId  ) ;
         add:  $>  .
     ^ res
  ] ifFalse:[ | name |
    name := self rubyFullName: envId .

    (name isNil or: [name size == 0]) ifTrue: [
      self isMeta ifTrue: [ ^ (destClass rubyFullName: envId), ':Class' ].
      ^ '#<Class:0x', self asOop hex, '>' ].

    ^ name
  ]
