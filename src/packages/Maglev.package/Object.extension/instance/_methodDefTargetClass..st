*maglev-runtime
_methodDefTargetClass: envId
  | cls | 
  cls := self .
  self isBehavior ifFalse:[ cls := self virtualClass ]. 
  cls isRubySingletonClass ifTrue:[ ^ cls ].
  [ cls isRubyVirtual ] whileTrue:[
     cls := cls rubySuperclass: envId
  ].
  ^ cls 

