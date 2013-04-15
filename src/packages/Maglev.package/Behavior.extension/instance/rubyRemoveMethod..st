*maglev-runtime
rubyRemoveMethod: aSymbol
  "a ruby primitive"
  | cm envId sel | 
  envId := 1"__callerEnvId" .
  sel := aSymbol _asSymbolWithRubySuffix: 16r3 " #0*& " .
  cm := self compiledMethodAt: sel rubyEnv: envId .
  aSymbol asString = 'const_missing' ifTrue: [
  ^ nil.
  ].
  cm ifNotNil:[
    cm _isSmallInteger ifTrue:[
       cm <= METH_lu_skipPersistentMd ifTrue:[
         NameError signal:'remove_method , ', aSymbol , ' has already been undef-ed'
       ].
    ].
    (self rubySelectorsWithPrefix: aSymbol env: envId) do: [ :eaSelector |
        self removeRubySelector: eaSelector env: envId .
    ] .
    self isRubyModuleFunctions ifTrue:[
      self primaryCopy @ruby1:singleton_method_removed: aSymbol .
      ^ self
    ].
    self isRubySingletonClass ifTrue:[
      self @ruby1:singleton_method_removed: aSymbol  .
      ^ self
    ].
    self @ruby1:method_removed: aSymbol  .
  ] ifNil:[
    NameError signal:'remove_method , ' , aSymbol , ' not found'
  ]

