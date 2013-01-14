*maglev-runtime
rubyAncestors
  "a ruby primitive"
  | arr cls  envId |
  envId := 1"__callerEnvId" .
  arr := { self }.
  cls := self .
  [ true ] whileTrue:[
     "ancestors of a Class stops at Kernel"
     cls := cls rubySuperclass:  envId .
     cls ifNil:[ ^ arr ].
     cls isRubyVirtual ifTrue:[ | pcopy |
       arr add: (pcopy := cls rubyPrimaryCopy).
       pcopy == Kernel ifTrue:[ ^ arr ]
     ] ifFalse:[
       (cls nameSpace: envId) ifNotNil:[ arr add: cls ]
                "ifNil:[ do not include a smalltalk class] ".
     ].
  ].

