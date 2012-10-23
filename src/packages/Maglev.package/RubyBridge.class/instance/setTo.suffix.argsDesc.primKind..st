*maglev-runtime
setTo: aSymbol suffix: aSuffix argsDesc: argsDescrInt primKind: primArg
  "aSuffix is suffix of aSymbol, i.e. suffix of the non-bridge method
  argsDescrInt is from argsNode of a RubyMethodDefNode's argsNode "
      "ruby_selector_suffix dependent"

  leaves := IdentityKeyValueDictionary new .
  to := aSymbol .
  toSuffix := aSuffix .
  primKnd := primArg .
  argsDescrInt ifNotNil:[ | fopt |
        neededArgs := argsDescrInt bitAnd: 16rFF .
        fopt := (argsDescrInt >> 8) bitAnd: 16rFF .
        fopt == 0 ifTrue:[ fopt := SmallInteger maximumValue ].
        firstOptArg := fopt .
        needsBlockArg := (argsDescrInt  bitAnd: 16r100000) ~~ 0 .
        needsRestArg := (argsDescrInt  bitAnd: 16r10000) ~~ 0 .  
  ] ifNil:[ 
    neededArgs := (aSuffix codePointAt: 2) - 48"$0" .
    needsRestArg  :=  (aSuffix at: 3) == $* .
    needsBlockArg :=  (aSuffix at: 4) == $& .
  ].

