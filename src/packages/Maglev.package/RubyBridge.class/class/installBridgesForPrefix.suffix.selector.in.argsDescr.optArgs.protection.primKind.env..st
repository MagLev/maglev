*maglev-runtime
installBridgesForPrefix: prefix suffix: destSuffix selector: rubySelArg
    in: aClass argsDescr: argsDescrInt
    optArgs: optArgsDescr protection: protInt primKind: primKind env: envId
 "primKnd argument is one of  0:normal 1:primitive 2:primitive_nobridge
                       4:boot 5:primitive during boot , 6: primitive_nobridge during boot .
  Returns prefix" 
 | suffixes generics sz templates useGenerics  aComp masks rubySel |
 templates := BridgeOptions atOrNil: envId .  "instances with fromSuffix predefined"
 templates ifNil:[
   templates := self initializeGenericBridges: envId
 ].
    "if we have optional args, then we must produce IR for a new bridge evaluate
     the optional args logic.  But if that IR would signal an ArgumentError ,
     and protInt==0, then   we use the precompiled generic method if one was found."
 ( optArgsDescr + protInt) == 0 ifTrue:[ useGenerics := true ].
 generics := (GenericErrCms at: envId) at: destSuffix otherwise: nil .
 suffixes := self suffixOptions . masks := self suffixOptionMasks .
 rubySel := rubySelArg ifNil:[  (prefix , destSuffix) asSymbol ].
 1 to: (sz := suffixes size) do:[ :n | | frSuffix  |
    frSuffix := suffixes at: n .
    frSuffix ~~ destSuffix ifTrue:[ | frSel cm genErrCm  |
      frSel :=  prefix _asSymbolWithRubySuffix: ( masks at: n ) .
      cm :=  primKind >=4 ifTrue:[ "during bootstrap,  don't overwrite existing variants"
        aClass compiledMethodAt: frSel rubyEnv: envId .
      ].
      cm ifNil:[
        generics ifNotNil:[  genErrCm := generics at: n ].
        useGenerics ifNotNil:[ cm := genErrCm ].
        cm ifNil:[ | br |
          (br := (templates at: n) shallowCopy ) 
             setTo: rubySel suffix: destSuffix argsDesc: argsDescrInt primKind: primKind .
          br from: frSel .
          aComp ifNil:[ aComp := RubyCompiler new ].
          cm := br cmForOptArgs: optArgsDescr protection: protInt class: aClass 
                     genericErrCm: genErrCm compiler: aComp env: envId .
        ].
          "frSel == #'=~:' ifTrue:[ self pause . SessionTemps current at:#TrapBR put: true
  ]."
        aClass addRubySelector: frSel method: cm  env: envId 
  
      ].
   ].
 ].
 ^ prefix

