*maglev-runtime
rubyUndefMethod: aName
  "a ruby primitive"
  | sel cm envId baseSel |
  envId := 1"__callerEnvId" .
  baseSel := aName asSymbol .
  sel := baseSel _asSymbolWithRubySuffix: 16r3 " #0*& " .
  cm := self compiledMethodAt: sel rubyEnv: envId .
  cm ifNil:[
     (self whichClassIncludesSelector: sel rubyEnv: envId) ifNil:[
        NameError signal:'undef_method, ', aName, ' not found'
     ]
  ] ifNotNil:[
     cm == METH_lu_undef_ed ifTrue:[ 
       "NameError signal:'undef_method, ', aName , ' has already been undef-ed'"
     ].
    (self rubySelectorsWithPrefix: baseSel env: envId) do: [ :eaSelector |
        self removeRubySelector: eaSelector env: envId .
    ] .
    self isRubyModuleFunctions ifTrue:[
      self primaryCopy @ruby1:singleton_method_undefined: baseSel .
      ^ self
    ].
    self isRubySingletonClass ifTrue:[
      self @ruby1:singleton_method_undefined: baseSel .
      ^ self
    ].
    self @ruby1:method_undefined: baseSel .
  ].
  RubyBridge suffixOptions do:[:sufix | | sel |   "fix Trac422"
     sel :=  (baseSel , sufix) asSymbol .
     self addRubySelector: sel method: 3"method-hidden" env: envId 
  ].

