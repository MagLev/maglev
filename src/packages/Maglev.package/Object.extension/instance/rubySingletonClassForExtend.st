*maglev-runtime
rubySingletonClassForExtend
  "a ruby primitive"
  | envId cls |
  envId := 1 "__callerEnvId" .
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[
       cls := self _rubyModuleIncludeSelfEnv: envId
    ].
  ] ifFalse:[
    self isBehavior ifTrue:[
       cls := self isMeta ifTrue:[ self _singletonClassFor: envId ]
                  ifFalse:[ self virtualClass "extending metaclass" ].
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls

