*maglev-runtime
walkInEval
  | mth |
   mth := RubyCompilerState current parentMethodDefOrNil .
  (mth ~~ nil and:[ mth ~~ self]) ifTrue:[ ^ mth  walkInEval ].
  ^ false

