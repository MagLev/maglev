*maglev-runtime
walkFixedInstVars: list forClass: aBody
 | ary sz |
 aBody class == RubyClassBodyNode ifFalse:[
   self signal: ArgumentError with: '__fixed_instvars only legal in a class body'
 ].
 sz := list size .
 sz == 0 ifTrue:[
   self signal: ArgumentError with: '__fixed_instvars requires at least one arg'
 ].
 1 to: sz do:[:j | | elem ecls |
   elem := list at: j  .
   ecls := elem class .
   (ecls == RubySymbolNode or:[ ecls == RubyStrNode]) ifTrue:[ | val |
     val := elem _value .
     ((val _rubyOrdAt: 0 ) == 64"$@" and:[(val _rubyOrdAt: 1) ~~ 64 ]) ifFalse:[
       self signal: ArgumentError
          with: 'illegal instVar name, element ' , j asString ,' of list'
     ]
   ] ifFalse:[
     self signal: ArgumentError
        with: 'args to __fixed_instvars must be String or Symbol constants' .
   ]
 ].
 (ary := RubyArrayNode _new) list: list copy .
 (aBody classNode fixedInstVars: ary) ifFalse:[ 
    self signal: ArgumentError with: 'second call to __fixed_instvars found in class body'
 ]

