*maglev-runtime
reset
  "resets environment 1"
  | found |
  found := Saved ~~ nil .
  " self reset: 2   with Melbourne parser, NO LONGER USED"
  self reset: 1 .
  Saved := nil .
  RubyCompiler parser: nil .
  SessionTemps current removeKey: #DefaultRubyContext ifAbsent: [] .
  ^ found

