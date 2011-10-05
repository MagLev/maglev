
doit
Object subclass: 'RubyArgs'
	instVarNames: #( scriptName rubyArgs scriptArgs
	                  libs requires scriptlets)
	classVars: #()
	classInstVars: #()
	poolDictionaries: #()
	inDictionary: ''
	category: 'MagLev-Runtime'
	options: #()

%

set class RubyArgs
removeallmethods
removeallclassmethods

set class RubyArgs class
category: 'as yet unclassified'
method:
forScript: scriptName withRubyArgs: rubyArgs withScriptArgs: scriptArgs
	"rubyArgs is an array of arguments to the VM.
	 scriptArgs is an array of arguments to the script"
	|instance envId|
	envId := 1 . "This is only called from maglev-ruby script"
	instance := self basicNew initialize .
	instance rubyArgs: rubyArgs .
	instance scriptArgs: scriptArgs .
	instance scriptName: scriptName .
	instance _parseRubyArgs: envId .
	^ instance .

%


set class RubyArgs
category: 'as yet unclassified'
method:
initialize
	libs := Array new .
	requires := Array new .
	scriptlets := Array new .
	

%


set class RubyArgs
category: 'accessing'
method:
libs

   "Return the value of the instance variable 'libs'."
   ^libs

%


set class RubyArgs
category: 'as yet unclassified'
method:
processArgsInto: context env: envId

	"Parse rubyArgs and scriptArgs, and set the context accordingly.
	 Sets $0, ARGV $LOAD_PATH (if passed -I) and will require all files 
	 named in -r parameters."
	
	self setGlobals: envId .
	self setContext: context env: envId .

%


set class RubyArgs
category: 'accessing'
method:
requires

   "Return the value of the instance variable 'requires'."
   ^requires

%


set class RubyArgs
category: 'as yet unclassified'
method:
rubyArgs
	^ rubyArgs

%


set class RubyArgs
category: 'as yet unclassified'
method:
rubyArgs: stringArray
	rubyArgs := stringArray.

%


set class RubyArgs
category: 'as yet unclassified'
method:
scriptArgs
	^ scriptArgs

%


set class RubyArgs
category: 'as yet unclassified'
method:
scriptArgs: stringArray
	scriptArgs := stringArray.

%


set class RubyArgs
category: 'accessing'
method:
scriptlets

   "Return the value of the instance variable 'scriptlets'."
   ^scriptlets

%


set class RubyArgs
category: 'as yet unclassified'
method:
scriptName
	^ scriptName

%


set class RubyArgs
category: 'as yet unclassified'
method:
scriptName: aName
	scriptName := aName

%


set class RubyArgs
category: 'as yet unclassified'
method:
setContext: context env: envId

	"Sets $LOAD_PATH (if passed -I) and will require all files 
	 named in -r parameters."
	self libs do:[ :each| context _addLibs: each env: envId ] .
	self requires do:[ :each| context requireFileNamed: each env: envId ] .
	^ self

%


set class RubyArgs
category: '*maglev-runtime'
method:
setGlobals: envId

    "Sets $0 and ARGV from receiver."
    | tns argv |
    tns := Object transientNameSpaceForStore: envId .
    scriptName ifNotNil:[ tns rubyGlobalVar: #'$0' put: scriptName ].
    argv := tns at:#ARGV .
    argv size > 0 ifTrue:[ self error:'ARGV is non empty in RubyArgs>>setGlobals:'].
    argv addAll: scriptArgs .
    ^ self 

%


set class RubyArgs
category: 'as yet unclassified'
method:
_extractArgFrom: argsArray into: anArray
	"Return the parameter to a command line argument that takes the
	 form  '-I' 'param'  or '-Iparam', and return 'param'.
	 Signal an error if param is missing.
	 Modifies argsArray to consume parameters."
	
	|arg sz result|
	arg := argsArray removeFirst .
	(arg beginsWith: '-') 
		ifFalse:[ self error: 'parameter should begin with - : ', arg printString ] .
	sz := arg size .
	result :=
		sz = 2 
			ifTrue:[ 
				argsArray isEmpty ifTrue:[ self error: arg printString, ' needs a parameter' ] .
				argsArray removeFirst ]
			ifFalse:[ 
				sz < 3 ifTrue:[ self error: arg printString, ' needs a parameter' ].
				arg copyFrom: 3 to: sz ] .
	anArray add: result .
	^ result .

%


set class RubyArgs
category: '*maglev-runtime'
method:
_parseRubyArgs: envId
  
    "rubyArgsArray is an array of all the command line parameters the ruby interpreter
    should process before running the ruby script. 
     Parse the args into the appropriate instvars.  Populates libs scriptlets and requires."
    
    | done processedArg args globals parseWarn |
    args := Array withAll: rubyArgs .
    done := false .
    parseWarn := false .
    globals := IdentityKeyValueDictionary new .
    processedArg := true .
    [ processedArg and: [ args size > 0 ]] whileTrue:[ |curArg|
        processedArg := false .
        curArg := args at: 1 .  "Peek, until we know we have consumed an arg"

        (curArg beginsWith: '-I') 
            ifTrue:[ 
                self _extractArgFrom: args into: libs .
                processedArg := true].
        
        (curArg beginsWith: '-e') 
            ifTrue:[
                self _extractArgFrom: args into: scriptlets .
                processedArg := true ] .
            
        (curArg beginsWith: '-r') 
            ifTrue:[
                self _extractArgFrom: args into: requires .
                processedArg := true ] .
            
        (curArg beginsWith: '-v') ifTrue: [ 
            globals at: #'$VERBOSE' put: true .   parseWarn := true .
            args removeFirst .
            processedArg := true ] .
        "note, --version processed in maglev-ruby shell script and VM not invoked"
        
        (curArg beginsWith: '-W') "Warning level: '-Wn', where 'n' is a single digit"
            ifTrue: [ |intVal|
                intVal := (curArg copyFrom: 3 to: 3) asInteger .
                globals at: #'$-W' put: intVal .
                intVal > 1 ifTrue:[ globals at: #'$VERBOSE' put:true .  parseWarn := true ]
                   ifFalse:[ intVal == 1 ifTrue:[ globals at: #'$VERBOSE' put: false ]
                                        ifFalse:[ globals at: #'$VERBOSE' put: nil ]  ].
                args removeFirst .
                processedArg := true ] .
            
        (curArg beginsWith: '-w') 
            ifTrue:[
                globals at: #'$VERBOSE' put: true .  parseWarn := true .
                globals at: #'$-w' put: true .
                args removeFirst .
                processedArg := true ] .
    ] .
    self _setGlobalsFrom: globals env: envId .
    SessionTemps current at:#MAGLEV_parseWarn put: parseWarn .

%


set class RubyArgs
category: 'accessing'
method:
_setGlobalsFrom: globals env: envId
	"For each of the key value pairs, set the corresponding transient global"
	| tns |
	tns := Object transientNameSpaceForStore: envId .
	globals keysAndValuesDo: [ :key :value | tns rubyGlobalVar: key put: value ] .
	^ self .

%

