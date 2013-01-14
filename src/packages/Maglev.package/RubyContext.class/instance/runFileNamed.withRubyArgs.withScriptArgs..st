*maglev-runtime
runFileNamed: aName withRubyArgs: rubyArgs withScriptArgs: scriptArgs
	^ self runFileNamed: aName 
		withRubyArgs: rubyArgs
		withScriptArgs: scriptArgs
		env: 1

