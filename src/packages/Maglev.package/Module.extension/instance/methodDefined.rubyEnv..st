*maglev-runtime
methodDefined: aSymbol rubyEnv: envId 
  "do not look in Object or Kernel .
   does not consider the package policy.
   Returns nil or an Array { cm . protectionOverride }"
  | cls cm override |
  cls := self .
  [
    (cm := cls compiledMethodAt: aSymbol rubyEnv: envId) ifNotNil:[
      cm _isSmallInteger ifTrue:[
        override ifNil:[ override := cm ].
      ] ifFalse:[
        ^ { cm . override ifNil:[ 0 ] }
      ].
    ].
    "include virtual classes for ruby modules. "
    cls := cls rubySuperclass: envId .
    cls ~~ Object  "stop before Object, assume virtualKernel above Object"
  ] whileTrue .
  ^ nil

