*maglev-runtime
_singletonClassFor: envId
   | cls |
   (cls := self virtualClass)  isRubySingletonClass ifFalse:[
      self addRubySingletonClass: envId .
      cls := self virtualClass .
      cls isRubySingletonClass ifFalse:[ self error:'_singletonClassFor:, creation failed' ].
   ].
   ^ cls

