*maglev-runtime
_bindingInfo: aBindingContext
  "A ruby primitive.
   argument is { aVariableContext . aMethodDefTarget, aGsNMethod .  ... }  from  Object>>_bindingContext:  .
   result is { aVariableContext . aMethodDefTarget. namesDict } ,
   where namesDict is a IdentityKeyValueDictionary 
      which maps   argOrTempName   to  anOffset 
   thus describing  slots in the VC applicable to a RubyBinding.
   The result is stored into aBinding.staticLink , aBinding.names by 
   Binding#initialize methods in  bootstrap/Binding.rb "

  | vc sz vcNamedSize  warnCba nDict |
  aBindingContext ifNil:[ ArgumentError signal:'in eval, missing aBindingContext' ].
  vc := aBindingContext at: 1 .
  vc ifNil:[ ArgumentError signal:'in eval , aBindingContext has no vc'].
  vcNamedSize := VariableContext instSize .
  sz := aBindingContext size .
  nDict := IdentityKeyValueDictionary new .
  3 to: sz do:[:n |  | nArgs names offsets osz aMeth  lastTmpIdx |
    aMeth := aBindingContext at: n . "nil if home meth not found"
    aMeth ifNotNil:[
      names := aMeth argsAndTemps .
      nArgs := aMeth numArgs.  "skip the args, meth/block will have copied all args to VC"
      offsets := aMeth argsAndTempsOffsets .  "offsets are zero-based absolute"
      lastTmpIdx  := (osz := offsets size) - aMeth _numCopyingBlockArgs .
      nArgs + 1 to: lastTmpIdx do:[:k | | aName ofs |
        aName := names at: k .
        (nDict includesKey: aName) ifFalse:[
          ofs := ((offsets at: k) + 1 - vcNamedSize) . "convert to 1-based varying"
          nDict at: aName put: ofs .
        ].
      ].
      lastTmpIdx < osz ifTrue:[
         ( lastTmpIdx == (osz - 1) and:[ (names at: osz) == #self ]) ifFalse:[
           warnCba := true
        ].
      ].
    ].
  ].
  " Let's be a little quieter about warnings.
    See discussion at: https://github.com/MagLev/maglev/issues/64
  warnCba ifNotNil:[ | msg |
    msg := 'binding for eval does not include CopyingBlockArgs yet' .
    Kernel @ruby1:warn: msg .
  ]. "
  ^ { vc . aBindingContext at: 2"methodDefTarget" . nDict  }.

