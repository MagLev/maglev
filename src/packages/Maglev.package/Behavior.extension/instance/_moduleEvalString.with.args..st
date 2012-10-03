*maglev-runtime
_moduleEvalString: aString with: vcGlobalsArr args: argsArr
    "A ruby primitive."
   | defStk envId  lexSelfStk aBinding cst rtModuStk cld |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: self .
  cld at: 7 put: self " _rubyThreadDataAt: 7 put: " .

  aBinding := argsArr at: 1 .
  cst := RubyCompilerState current .
  lexSelfStk := cst lexicalSelfStack .
  lexSelfStk push: nil . " fix Trac908a" 
  rtModuStk := cst rtModuleStack .
  rtModuStk push: self .
  ^ [ | file line | 
       aBinding setModuleEval .
       file := argsArr atOrNil: 2 .
       file _stringCharSize == 0 ifTrue:[ file := nil ].
       line := argsArr atOrNil: 3 .
       line _isSmallInteger ifFalse:[ line := 1 ].
       RubyCompiler new evaluateString: aString binding: aBinding with: vcGlobalsArr 
       fileName: file lineNumber: line  env: envId 
    ] ensure: [
      defStk pop: self .
      lexSelfStk pop: nil .
      rtModuStk pop: self .
    ]

