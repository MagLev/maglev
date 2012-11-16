*maglev-runtime
whichClassIncludesSelector: aString rubyEnv: envId

  "does not consider the package policy, 
   ignores protection and protection overrides "
  | cls aSymbol |
  aSymbol := Symbol _existingWithAll: aString .
  aSymbol ifNil:[ ^ nil ].
  cls := self .
  [ | cm |
    (cm := cls compiledMethodAt: aSymbol rubyEnv: envId) ifNotNil:[
       cm _isSmallInteger ifFalse:[ ^ cls ].
       cm == METH_lu_undef_ed ifTrue:[ ^ nil "stop lookup here"].
    ].
    "include virtual classes for ruby modules. "
    cls := cls rubySuperclass: envId .
    cls ~~ nil
  ] whileTrue .
  ^ nil

