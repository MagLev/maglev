*maglev-runtime
rubyAlias: newName from: oldName
  "a ruby primitive.
   target of alias may be private, the NoMethodError is thrown
     on attempt to execute, not at the point of aliasing "
      "ruby_selector_suffix dependent"
   | cls oldSymbol newSymbol oldSel envId cst selectors |
   envId := 1"__callerEnvId" .
   oldSymbol := oldName asSymbol .
   newSymbol := newName asSymbol .
   cst := RubyCompilerState current .
   (RubyCompiler reimplementationAllowed: newSymbol for: self cst: cst ) ifFalse:[
      NameError signal:'aliasing of ' ,  newSymbol , ' not allowed '
   ].
   oldSel := oldSymbol _asSymbolWithRubySuffix: 16r3 " #0*& " .
   cls := self whichClassIncludesSelector: oldSel rubyEnv: envId .
   cls ifNil:[ NameError signal: 'alias_method:  no method found for ', oldSymbol ].
   
   selectors := cls rubySelectorsWithPrefix: oldSymbol env: envId .
   selectors do: [:oldSelector | | cm |
    
     cm := cls compiledMethodAt: oldSelector rubyEnv: envId .
     cm ifNotNil:[
        | mCopy newSelector suffix commStr |
        suffix := oldSelector rubySelectorSuffix .
        newSelector := (newSymbol, suffix) asSymbol .
        "copy even if not a bridge, so protection can be changed on the copy"
        commStr := '
METHOD COPIED by alias' .
        cm isRubyBridgeMethod ifTrue:[
          mCopy := cm _copyForClass: self aliasFrom: oldSymbol to: newSymbol comment: commStr.
        ] ifFalse:[
          "no bytecode level changes, just copy so we can change protection. Trac 919"
          mCopy := cm _copyForClass: self aliasFrom: nil to: nil comment: commStr.
        ].
        mCopy environmentId ~~ 0 ifTrue:[
          mCopy selector: newSelector . "for 1.8.7"
          mCopy _rubyInClass: cls ."preserve original for use by SEND_RUBY_SUPER, Trac676"
        ].
        mCopy immediateInvariant .
        self addRubySelector: newSelector method: mCopy env: envId .
    ] .
  ] .

