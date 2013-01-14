ruby-support
astFor: fakeFile
  "This method calls the parser on the source code in fakeFile and returns the ast.
   It emulates RubyCompiler parseFileNamed:loadName:"
  | trace warn  tmps source|

  tmps := SessionTemps current .
  trace := tmps at: #MagRpDEBUG otherwise: 0 .
  warn := tmps at:#MAGLEV_parseWarn otherwise: false .
  trace ~~ 0 ifTrue:[ GsFile gciLogServer:'Mel parse ' , (fakeFile fullPath) ].

  "The following:
      ast := RubyParserM rpParseFile: fullPath  loadName: aName yTrace: trace - 1 warnings: warn
      ^ ast
   reads the source out of the file, then calls rpParseString...
   result is after walkWithScopes has been done"

   source := fakeFile source .
   source immediateInvariant .
   ^ RubyParserM rpParseString: source
                 path: (fakeFile fullPath)
                 loadName: (fakeFile fullPath)
                 yTrace: trace -1
                 warnings: warn

