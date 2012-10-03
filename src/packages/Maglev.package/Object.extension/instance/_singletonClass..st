*maglev-runtime
_singletonClass: envId 
  | cls |
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[ 
       cls := self _rubyModuleIncludeSelfEnv: envId 
    ].
  ] ifFalse:[  
    self isBehavior ifTrue:[ 
       cls := self virtualClass . "extending metaclass"
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls 

