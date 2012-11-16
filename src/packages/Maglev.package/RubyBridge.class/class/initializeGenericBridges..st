*maglev-runtime
initializeGenericBridges: envId
  "creates and returns the bridge options for specified environment"
  |  bridges suffixes sz generics comp frSym aBr  |
  comp :=   RubyCompiler new .
  suffixes := self suffixOptions.
  sz := suffixes size .
  bridges := Array new: sz .
  1 to: sz do: [:n |
   frSym := suffixes at: n .
   (aBr := RubyBridge _basicNew) fromSuffix: frSym ;  immediateInvariant .
   bridges at: n put: aBr
  ].
  bridges immediateInvariant .
  BridgeOptions _rubyAt: envId - 1 put: bridges  .
  generics := IdentityKeyValueDictionary new .  "key toSuffix,
        value is Array of genericErrCms or nil for each fromSuffix in suffixOptions "
  1 to: sz do:[ :m | | toSym cmArr |
   toSym := suffixes at: m .
   cmArr := Array new: sz .
   1 to: sz do:[:k | |   cm ir  |
    frSym := suffixes at: k .
    frSym ~~ toSym ifTrue:[
        (aBr := (bridges at: k) shallowCopy) setTo: ('__meth', toSym) asSymbol
                                 suffix: toSym argsDesc: nil primKind: 0 .
        (ir :=  aBr irGenericErrMethod: ('__meth', frSym) asSymbol
                    env: envId ) ifNotNil:[
         ir class: Object ; addMethodProtection: 0 .
           cm :=  comp  compiledMethodForIR: ir .
           cm ifNotNil:[
               cmArr at: k put: cm .
               GenericErrCmSet add: cm .
             ].
        ].
      ].
   ].
     cmArr immediateInvariant .
     generics at: toSym put: cmArr .
  ].
  GenericErrCms _rubyAt: envId - 1 put: generics .
  ^ bridges

