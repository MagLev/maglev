parsetree
optionsFromString: aString
       "result must agree with options arg to om::RegexCompile in regexprim.c"
  | optsInt |
  optsInt := 0 .
  1 to: aString size do:[:j | | ch ofs sym |
    ch := aString at: j .
    (ofs :=  #( $i $o $m $x ) indexOfIdentical: ch) ~~ 0 ifTrue:[
       "literal array must agree with   oniguruma.h ."
       " can't find any support for SUBSTITUTE_ONCE in oniguruma.h"
       sym := #( #IGNORECASE nil #MULTILINE #EXTEND ) at: ofs .
       sym == nil ifTrue:[ self error: 'Regexp option ', ch ,' not supported'].
       optsInt := optsInt bitOr: (Regexp classVarAt: sym) .
    ].
    (ofs := #( $n $e $s $u ) indexOfIdentical: ch) ~~ 0 ifTrue:[
       "literal array of KCODE_ must agree with C defs in regexprim.hf "
       sym := #( #KCODE_NONE #KCODE_EUC #KCODE_SJIS #KCODE_UTF8 ) at: ofs .
       optsInt := optsInt bitOr: ((Regexp classVarAt: sym) bitShift: 32) .
    ].
  ].
  ^ optsInt

