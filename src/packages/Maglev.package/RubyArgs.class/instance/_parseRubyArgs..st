*maglev-runtime
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
            globals at: #'$VERBOSE' put: true .
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

