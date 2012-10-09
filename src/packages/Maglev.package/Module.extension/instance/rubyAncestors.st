*maglev-runtime
rubyAncestors
  "a ruby primitive"
  | arr cls  envId |
  envId := 1"__callerEnvId" .
  arr := { self }.
  cls := self .
  [ true ] whileTrue:[
     "ancestors of a Module stops before Object"
     cls := cls rubySuperclass:  envId .
     (cls == nil or:[ cls == Object]) ifTrue:[ ^ arr ] .
     cls isRubyVirtual ifTrue:[
       arr add: cls rubyPrimaryCopy
     ] ifFalse:[
       (cls nameSpace: envId) ifNotNil:[ arr add: cls ]
                "ifNil:[ do not include a smalltalk class] ".
     ].
  ].

