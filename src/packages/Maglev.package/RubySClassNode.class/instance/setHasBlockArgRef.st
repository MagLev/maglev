*maglev-runtime
setHasBlockArgRef
  | mth |
   mth := RubyCompilerState current parentMethodDefOrNil .
  (mth ~~ nil and:[ mth ~~ self]) ifTrue:[ ^ mth  setHasBlockArgRef ].
  ^ false

