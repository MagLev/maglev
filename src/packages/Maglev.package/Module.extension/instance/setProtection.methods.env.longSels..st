*maglev-runtime
setProtection: protInt methods: selectorsArray env: envId longSels: longSelectors 
 "protInt is 0 public, 1 protected, 2 private.
  Returns a SmallInteger, the number of selectors in selectorsArray
  for which protection was changed"
      "ruby_selector_suffix dependent"
 | sz count |
 count := 0 .
 (sz := selectorsArray size) == 0 ifTrue:[  | tns |
    "this path not used for meta classes/modules"
  "set default visibility for subsequent compilations, and shutoff _module_methods_all"
    self rubyMethodProtection: protInt env: envId .
    self disableModuleMethodsAll: envId .
 ] ifFalse:[
   1 to: sz do:[:n | | aName baseSel genSel cm found |  
     found := false .
     aName := selectorsArray at: n .
     baseSel := aName asSymbol "do not send rubySelectorPrefixSymbol" .
     genSel := baseSel _asSymbolWithRubySuffix: 16r3 " #0*& " .
     cm := self compiledMethodAt: genSel rubyEnv: envId .
     cm ifNil:[  | ary |
       ary := self lookupSelector: genSel rubyEnv: envId .
       ary ifNotNil:[ cm := ary at: 1 ].
       " if cm not nil here, we are adding a protection override"
     ].
     cm ifNotNil:[ | masks genericBridgeCms longSels override |
       genericBridgeCms := RubyBridge genericErrCmSet .
       masks := RubyBridge suffixOptionMasks .
       1 to: masks size do:[ :n | | mask  sel mth |
         mask := masks at: n .
         sel := baseSel _asSymbolWithRubySuffix: mask .
         mth := self compiledMethodAt: sel rubyEnv: envId .
         mth ifNotNil:[
           mth _isSmallInteger ifTrue:[
             mth >= METH_prot_override_private ifTrue:[ 
               "change a protection override"
               override := METH_prot_override_public - protInt .
               self addRubySelector: sel method: override env: envId .
               found := true .
             ] ifFalse:[
               "skip an undef-ed method"
             ] 
           ] ifFalse:[
             (genericBridgeCms includes: mth) ifTrue: [ |newCm|
                "Do not change protection on generic bridge methods, as that
                 will affect all other classes that also share that method"
                newCm := mth _copyForClass: self aliasFrom: nil to: nil comment: nil.
                newCm setRubyProtection: protInt .
                newCm immediateInvariant .
                self addRubySelector: sel method: newCm env: envId .
                found := true .
              ] ifFalse: [
                mth setRubyProtection: protInt .
                found := true .
              ] . 
            ]
          ] ifNil:[  "add a protection override"
            override := METH_prot_override_public - protInt .
            self addRubySelector: sel method: override env: envId .
            found := true .
          ]
       ].
       longSels := longSelectors ifNil:[  self selectors4moreArgs: baseSel env: envId ].
       1 to: longSels size do:[:n | | sel mth |
         sel := longSels at: n.
         mth := self compiledMethodAt: sel rubyEnv: envId .
         mth ifNotNil:[
           mth _isSmallInteger ifTrue:[
             mth >= METH_prot_override_private ifTrue:[ 
               "change a protection override"
               override := METH_prot_override_public - protInt .
               self addRubySelector: sel method: override env: envId .
               found := true .
             ] ifFalse:[
               "skip an undef-ed method"
             ]
           ] ifFalse:[
             mth setRubyProtection: protInt .
             found := true .
           ].
         ]
       ].
     ] "ifNil:[  ignore methods not found ]" .
     found ifTrue:[ count := count + 1 ].
   ].
 ].
 ^ count

