*maglev-runtime
_singletonClass: envId  
  | cls |
  self class == Module 
    ifTrue: ["a  class<<self   within a  module "
      cls := self moduleMethodsModuleOrNil ifNil: [cls := self _rubyModuleIncludeSelfEnv: envId]] 
    ifFalse: ["a class or a non-class object"
      ^ self _singletonClassFor: envId].
  ^ cls

