*maglev-runtime
instanceEvalString: aString with: vcGlobalsArr args: argsArr
    "A ruby primitive.
     instance_eval comes here.
     Evaluate aString with self set to this object"
  | defStk envId lexSelfStk  defnTarget  selfCls cld |
  envId := 1"__callerEnvId" . 
  selfCls := self virtualClass   .
  defnTarget := self _singletonClass: envId .
  cld := GsProcess _current clientData .
  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: defnTarget .
  cld at: 7 put: defnTarget " _rubyThreadDataAt: 7 put: " .

  selfCls == Object ifTrue:[ selfCls := nil ].  "don't need to include top-level"
  (lexSelfStk := RubyCompilerState current lexicalSelfStack ) push: selfCls .
  ^ [  | file line |
      file := argsArr atOrNil: 2 .
      file _stringCharSize == 0 ifTrue:[ file := nil ].
      line := argsArr atOrNil: 3 .
      line _isSmallInteger ifFalse:[ line := 0 ].
      RubyCompiler new evaluateString: aString binding: (argsArr at: 1) with: vcGlobalsArr 
        fileName: file lineNumber: line  env: envId
    ] ensure:[
      defStk pop: defnTarget.
      lexSelfStk pop: selfCls .
    ]

