*maglev-runtime
evalAddFileLineArgs: args for: aSelector
   | nargs |
   nargs := args size . 
   aSelector == #eval ifTrue:[
     nargs < 4 ifTrue:[
       self evalLineArg ifNotNil:[:lnum | args _rubyAt: 3 put: lnum ].
       nargs < 3 ifTrue:[
	 nargs < 2 ifTrue:[
	   args _rubyAt: 1 put: RubyNilNode newForIr .
	 ].
	 self evalFileArg ifNotNil:[:fn | args _rubyAt: 2 put: fn ].
       ].
     ].
   ] ifFalse:[  "note class_eval handled same as module_eval"
     self hasExplicitBlockArg ifTrue:[
	"do nothing for instance_eval or module_eval of a block" 
     ] ifFalse:[
       "instance_eval of string, module_eval"
       nargs < 3 ifTrue:[
	 self evalLineArg ifNotNil:[:lnum | args _rubyAt: 2 put: lnum ].
	 nargs < 2 ifTrue:[
	   self evalFileArg ifNotNil:[:fn | args _rubyAt: 1 put: fn ].
	 ].
       ] .
     ]  
   ]

