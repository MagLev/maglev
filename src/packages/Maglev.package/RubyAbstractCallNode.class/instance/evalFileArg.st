*maglev-runtime
evalFileArg
   | cst |
   cst := RubyCompilerState current .
   cst topMethodDefOrNil ifNotNil:[  :mth | |  f n |
     f := mth fileName .
     n := mth lineForOffset: position  .
     ^ RubyStrNode s_a: '(eval) from line ', n asString , ' of ' , f .
   ].
   ^ nil

