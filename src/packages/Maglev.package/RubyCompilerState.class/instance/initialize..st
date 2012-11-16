*maglev-runtime
initialize: envArg
   | stkClass |
   envId := envArg . 
   fileStack ifNil:[
     fileStack  :=  (stkClass := RubyCompilerStack) new .
     scopeStack := stkClass new .
     lexLevel := -1 .
     lexLevelStack := stkClass new .
     compilerStack := stkClass new .
     methStack := stkClass new .
     lineBiasStack := stkClass new .
     loopStack:= stkClass new .
     lastExceptionStack := stkClass new .
     "outerDefLexPath left as nil "
     parserStack := stkClass new .
     evalLexicalSelfStack := stkClass new .
   ].

