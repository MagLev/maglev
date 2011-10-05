
set class RubyBinding class
category: '*maglev-runtime'
method:
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
  warnCba ifNotNil:[ | msg |
    msg := 'binding for eval does not include CopyingBlockArgs yet' .
    Kernel @ruby1:warn: msg .
  ].
  ^ { vc . aBindingContext at: 2"methodDefTarget" . nDict  }.

%


set class RubyBinding
category: 'as yet unclassified'
method:
context
  ^ staticLink

%


set class RubyBinding
category: 'as yet unclassified'
method:
forModuleEval
  ^ forModuleEval

%


set class RubyBinding
category: '*maglev-runtime'
method:
homeMethod
  ^ homeMeth

%


set class RubyBinding
category: 'as yet unclassified'
method:
lexicalPath
  ^ lexicalScope

%


set class RubyBinding
category: '*maglev-runtime'
method:
locationForExistingName: aSymbol
  "compile time access , returns a SmallInteger , aSymbol, or nil"
  | d ofs |
  (d := names) ifNotNil:[
    (ofs := d at: aSymbol otherwise: nil) ifNotNil:[ 
      ^ ofs  "offset of the temp in the VariableContext"
    ]   
  ].
  (d := tmpsDict) ifNotNil:[
    (d at: aSymbol otherwise: nil) ifNotNil:[ 
      ^ aSymbol  "temp exists after dynamic addition"
    ]   
  ].
  ^ nil

%


set class RubyBinding
category: '*maglev-runtime'
method:
methodDefTarget 
  "used by Kernel#eval, but not other eval paths"
  ^ methDefTarget

%


set class RubyBinding
category: '*maglev-runtime'
method:
names
  ^ names  "an IdentityKeyValueDictionary or nil"

%


set class RubyBinding
category: 'as yet unclassified'
method:
selfObj
  ^ selfObj

%


set class RubyBinding
category: 'as yet unclassified'
method:
setModuleEval
  forModuleEval := true

%


set class RubyBinding
category: '*maglev-runtime'
method:
_block
  ^ block

%


set class RubyBinding
category: '*maglev-runtime'
method:
_evalPrePostSources
 "Returns an Array of pre, post, and vc name components of source string
   for use in eval with a binding"
|  usesTheBinding |
usesTheBinding := true . "always, for Trac 829"

^ { ' '. ' ' . '__vc' . usesTheBinding }

%


set class RubyBinding
category: '*maglev-runtime'
method:
_tempAt: aSymbol
  "runtime access for LocalVarNode in an eval,
    accessing a temp not in the VariableContext "
  ^ tmpsDict ifNotNil:[ :dict | dict at: aSymbol otherwise: nil ]

%


set class RubyBinding
category: '*maglev-runtime'
method:
_tempAt: aSymbol put: aValue
  "runtime store for LocalAsgnNode in an eval,
    to a temp created after the VariableContext was defined.
   Returns aValue. "
  | dict |
  (dict := tmpsDict) ifNil:[ 
    dict := IdentityKeyValueDictionary new .
    tmpsDict := dict
  ].
  dict at: aSymbol put: aValue .
  ^ aValue

%

