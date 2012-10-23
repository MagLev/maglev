as yet unclassified
forScript: aScriptName withRubyArgs: aRubyArgs withScriptArgs: aScriptArgs
	"rubyArgs is an array of arguments to the VM.
	 scriptArgs is an array of arguments to the script"
	|instance envId|
	envId := 1 . "This is only called from maglev-ruby script"
	instance := self basicNew initialize .
	instance rubyArgs: aRubyArgs .
	instance scriptArgs: aScriptArgs .
	instance scriptName: aScriptName .
	instance _parseRubyArgs: envId .
	^ instance .

