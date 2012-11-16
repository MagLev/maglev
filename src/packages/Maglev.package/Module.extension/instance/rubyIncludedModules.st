*maglev-runtime
rubyIncludedModules
  "a ruby primitive"
  | arr cls envId |
  envId := 1"__callerEnvId" .
  arr := {  }.
  cls := self rubySuperclass: envId .
  [ true ] whileTrue:[
     cls ifNil:[ ^ arr ] .
     cls == Object ifTrue:[ ^ arr ]. "stop at Object for Modules"
     cls isRubyVirtual ifTrue:[ | primary |
          (primary := cls rubyPrimaryCopy) class == Module ifTrue:[
               arr add: primary
           ].
     ]. 
     cls := cls rubySuperclass: envId 
  ].

