*maglev-runtime
rpParseString: sourceString scope: aScopN lineBias: bias evalScope: aRubyEvalScope fileName: fName
  | ast root trace tmps warn |
  sourceString immediateInvariant .
  tmps := SessionTemps current .
  trace := tmps at: #MagRpDEBUG otherwise: 0 .
  warn := tmps at:#MAGLEV_parseWarn otherwise: false .
  ast :=  self parse: sourceString cBytes: ( CByteArray withAll: sourceString)
       line: (bias < 0 ifTrue:[ 0 ] ifFalse:[ bias ])
       file: fName yTrace: trace - 1 warnings: warn 
       evalScope: aRubyEvalScope .
  ast _stringCharSize ~~ 0 ifTrue:[ RubyParseError signal: ast ].
  (root := RubyRootNode _basicNew)
        bodyNode:  ast ;
        lineBias: bias  ;
        fileName: '<eval>'  source: sourceString ;
        position: 1 .  "first one-based byte of file"
   "walkPositions no longer needed"
   (SessionTemps current at: #MAGLEV_logSexp  otherwise: nil) == true ifTrue:[
      GsFile gciLogServer: root _inspect .
   ].
   root walkScopes: aScopN  .
   ^ root

