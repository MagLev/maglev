*maglev-runtime
defineMethod: aSymbol block: blockArg  env: envId
  | newSym ir cm comp protInt aBlock  optBits newBlkMeths res |
      "ruby_selector_suffix dependent"
  "do not  aSymbol rubySelectorPrefix - fix Trac 913 "
  newSym := aSymbol _asSymbolWithRubySuffix: 16r3 " (prefix , '#0*&') asSymbol " .
  "take a copy of block and it's VariableContext"
  aBlock := blockArg _copyForRuby: aSymbol newBlockMethsInto: (newBlkMeths := { } ) .
  ir := RubyBridge irMethod_defineMethStarArgs: aSymbol block: aBlock inClass: self  
            env: envId .
  ir addMethodProtection: (protInt := self rubyMethodProtection: envId  ).
  comp := RubyCompiler new .
  blockArg _fileAndLine ifNotNil:[ :fl | ir fileName: (fl at: 1) ; lineNumber: (fl at: 2) ].
  cm := comp compiledMethodForIR: ir .
  1 to: newBlkMeths size do:[:j | 
     (newBlkMeths at: j) _rubyInClass: cm ; immediateInvariant "Trac748"
  ].
  RubyBridge installBridgesForPrefix: aSymbol suffix: #'#0*&' selector: newSym
     in: self argsDescr: nil 
     optArgs: (optBits := cm rubyOptArgsBits)  protection: protInt primKind: 0 env: envId .
  (self allModuleMethodsEnabled: envId) ifTrue:[ | mmMod |
     (mmMod := self moduleMethodsModule)  addRubySelector: newSym method: cm env: envId .
     RubyBridge installBridgesForPrefix: aSymbol suffix: #'#0*&' in: mmMod  argsDescr: nil 
         optArgs: optBits  protection: protInt primKind: 0 env: envId .
  ].
  self addRubySelector: newSym  method: cm env: envId  .
  (res := RubyUnboundMeth _basicNew) method: cm env: envId selPrefix: aSymbol .
  ^ res

