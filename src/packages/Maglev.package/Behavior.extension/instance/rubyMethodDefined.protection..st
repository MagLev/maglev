*maglev-runtime
rubyMethodDefined: aSymbol protection: protInt
  "a ruby primitive"
      "ruby_selector_suffix dependent"
  | arr envId |
  envId := 1"__callerEnvId" .
  arr := self methodDefined: (aSymbol prefixIfRubySelector _asSymbolWithRubySuffix: 16r3 " #0*&")
            rubyEnv: envId .
  arr ifNil:[ ^ false ].
  protInt ~~ -1 ifTrue:[ | override cm |
    override := arr at: 2 .
    override == 0 ifFalse:[
      override := METH_prot_override_public - override . "convert 7..5 to 0..2"
      ^ override == protInt 
    ].
    cm := arr at: 1 .
    ^ cm rubyMethodProtection == protInt 
  ].
  ^ true

