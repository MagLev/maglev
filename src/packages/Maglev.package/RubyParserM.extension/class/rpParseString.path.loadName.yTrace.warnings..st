*maglev-runtime
rpParseString: sourceString path: fullPath  loadName: aName yTrace: yTrace warnings: warn
  | root ast |
  ast := self parse: sourceString cBytes: ( CByteArray withAll: sourceString)
               line: 0 file: aName yTrace: yTrace warnings: warn
	       evalScope: nil .
  ast _stringCharSize ~~ 0 ifTrue:[ RubyParseError signal: ast ].
  (root := RubyRootNode _basicNew)
        bodyNode:  ast ;
        lineBias: 0  ;
        fileName: fullPath  source: sourceString ;
        position: 1 .  "first one-based byte of file"
  (SessionTemps current at: #MAGLEV_logSexp  otherwise: nil) == true ifTrue:[
      GsFile gciLogServer: root _inspect .
  ].
  root walkScopes: Object  .
  ^ root

