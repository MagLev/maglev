*maglev-runtime
_eval: aString binding: aBinding with: vcGlobalsSelf fileName: aFileName lineNumber: anInt 
    "A ruby primitive.
     Evaluate aString.  vcGlobals is an Array of size 3"
  | defStk lexSelfStk envId cld aClass |
  envId := 1"__callerEnvId" .
  cld := GsProcess _current clientData .
  aClass := aBinding methodDefTarget .

  (defStk := cld at: 3 " _rubyThreadDataAt: 3" ) push: aClass .
  cld at: 7 put: aClass " _rubyThreadDataAt: 7 put: " .

  (lexSelfStk := RubyCompilerState current lexicalSelfStack ) push: nil .
  ^ [
       RubyCompiler new evaluateString: aString binding: aBinding with: vcGlobalsSelf 
           fileName: aFileName lineNumber: anInt  env: envId
    ] ensure:[
       defStk pop: aClass . 
       lexSelfStk pop: nil .
    ].

