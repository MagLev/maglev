*maglev-runtime
defineMethod: aSymbol method: aMethod env: envId
  | mCls newCm protInt mSel mPrefix  newSym cm mSelSiz prefixSiz res |
    "Return a RubyUnboundMeth encapsulating a copy of aMethod,
     with selector of the copy changed to match aSymbol. " 
      "ruby_selector_suffix dependent"
  (aMethod isKindOf: RubyUnboundMeth) ifFalse:[
    ArgumentTypeError signal:'method argument is not a Method'
  ].
  cm := aMethod nonBridgeGsMethod: envId .
  mCls := cm inClass .
  self allInstVarNames size >= mCls allInstVarNames size ifFalse:[
     ArgumentError signal: 'define_method , arg''s class has more fixed instvars than self'
  ].
  newCm := cm _copyForClass: self aliasFrom: nil to: nil comment:'
METHOD COPIED by define_method' .  
  mPrefix := (mSel := newCm selector ) prefixIfRubySelector asString .
  mSelSiz := mSel size .
  prefixSiz := mPrefix size .
  newSym := aSymbol .  "Fix Trac 913"
  prefixSiz < mSelSiz  ifTrue:[ 
    newSym := newSym , (mSel copyFrom: prefixSiz + 1  to: mSelSiz) .
    newSym := newSym asSymbol 
  ] ifFalse: [ "prefixSiz = mSelSiz"
    "need to find target ruby bridge method suffix"
    "XXX: Does not work for args > 9, and not for primitives with Ruby implicit
    block args"
    | stNArgs |
    newSym := (newSym, '#', newCm numArgs asString, '__') asSymbol. "no splat, no block arg"
  ].
  newCm environmentId ~~ 0 ifTrue:[ 
    newCm selector: newSym ;
       setRubyProtection: ( protInt := self rubyMethodProtection: envId  ) . 
  ] ifFalse:[ 
    "cm was a smalltalk method from RHS of a ruby primitive, can't change selector" 
    protInt := 0 .
  ]. 
  newCm immediateInvariant .
  RubyBridge installBridgesFor: newSym in: self argsDescr: nil 
     optArgs: newCm rubyOptArgsBits  protection: protInt primKind: 0 env: envId . 
  self addRubySelector: newSym method: newCm env: envId  .
  (res := RubyUnboundMeth _basicNew) method: newCm env: envId selPrefix: mPrefix .
  ^ res

