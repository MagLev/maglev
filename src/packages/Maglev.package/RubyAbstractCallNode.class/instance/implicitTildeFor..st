*maglev-runtime
implicitTildeFor: aSymbol
  aSymbol == #last_match ifTrue:[  | ref |
    "For  Regexp.last_match   classmethod"
    ( ref := RubyVcGlobalNode _basicNew ) name:  #'$~'  .
     ^ ref
  ].
  ^ nil 

