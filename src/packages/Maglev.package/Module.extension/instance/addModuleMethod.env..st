*maglev-runtime
addModuleMethod: aSelector env: envId 
  "  Implement the Ruby instance method   module_function   for Module "
      "ruby_selector_suffix dependent"
  aSelector _isSymbol ifTrue:[
    "Add entries for specified selector and all related bridge methods
       to class-side module"
    | cm base moduleMethsMod masks longSels  |
    base := aSelector prefixIfRubySelector .
    cm := self compiledMethodAt: (base _asSymbolWithRubySuffix: 16r3 " #0*& ")
                     rubyEnv: envId .
    cm ifNil:[  NameError signal:'no such method ', aSelector ].
    cm _isSmallInteger ifTrue:[
      NameError signal:'method ', aSelector , ' was undef-ed or protection overridden'
    ].
    moduleMethsMod := self moduleMethodsModule .
    masks := RubyBridge suffixOptionMasks . 
    1 to: masks size do:[ :n | |  meth fullSel |
      fullSel :=  base _asSymbolWithRubySuffix: ( masks at: n ).
      meth := self compiledMethodAt: fullSel rubyEnv: envId .
      meth ifNotNil:[ | mmth |
        mmth := meth _copyForClass: self aliasFrom: nil to: nil comment:'
COPIED by module_function' .
       mmth setRubyProtection: 0 . "ensure public"
       mmth immediateInvariant .
        moduleMethsMod addRubySelector: fullSel  method: mmth env: envId .
        meth canBeWritten ifFalse:[ |mcpy |
           mcpy := meth _copyForClass: self aliasFrom: nil to: nil comment:'
COPIED by module_function to get authorization to make private'.
         mcpy immediateInvariant .
         self addRubySelector: fullSel method: mcpy env: envId .
        ].
      ].
    ].
    longSels := self selectors4moreArgs: base env: envId .
    1 to: longSels size do:[:m | | aSel meth |
       meth := self compiledMethodAt: (aSel := longSels at:m) rubyEnv: envId .
       meth ifNotNil:[ | mmth |
         mmth := meth _copyForClass: self aliasFrom: nil to: nil comment:'
COPIED by module_function' .
        mmth setRubyProtection: 0 . "ensure public"
        mmth immediateInvariant .
         moduleMethsMod addRubySelector: aSel  method: mmth env: envId .
       ].
     ].
    self setProtection: 2"private" methods: { base } env: envId longSels: longSels. "make original method private"
  ] ifFalse:[
    aSelector == true ifTrue:[
      "Create the class-side module, and set flag so all subsequent method defs
       get replicated in the class-side module."
      self moduleMethodsModule .  
      (self transientNameSpaceForStore: envId) at: #_module_methods_all compilePut: true .
    ] ifFalse:[ 
      aSelector == false ifTrue:[ self disableModuleMethodsAll: envId ]
             ifFalse:[ ArgumentError signal:'Module>>addModuleMethod: , invalid argument' ].
    ].
  ].

