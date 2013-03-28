*maglev-runtime
_singletonClass: envId 
  | cls |
  self class == Module ifTrue:[   "a  class<<self   within a  module "
    cls := self moduleMethodsModuleOrNil ifNil:[ 
       cls := self _rubyModuleIncludeSelfEnv: envId 
    ].
  ] ifFalse:[  
    (self isBehavior and: [self isMeta not and: [self isRubySingletonClass not]]) ifTrue:[ 
       "It's neither a meta class nor a Ruby singleton class, so it must be a class.
       Smalltalk meta classes (=first level Ruby singleton class) are automatically
       created by GemStone."
       cls := self virtualClass.
    ] ifFalse:[ cls := self _singletonClassFor: envId ].
  ].
  ^ cls

