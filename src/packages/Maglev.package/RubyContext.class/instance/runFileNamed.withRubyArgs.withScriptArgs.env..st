*maglev-runtime
runFileNamed: aName withRubyArgs: rubyArgs withScriptArgs: scriptArgs env: envId

  "This is called by maglev-ruby shell script to run a ruby file, aName.
   rubyArgs is an array of parameters to maglev (e.g., '-I' 'a/path' '-r')
   scriptArgs is an array of parameters passed to the script in ARGV.
   Setup the ARGV array, $0 and run the script. 
   Do NOT use loadFileNamed etc,,as they will try to find aString on the load path."
        
   | args commitFlag retValue |
   commitFlag := (SessionTemps current at: #MAGLEV_commitFlag otherwise: false) .
   args := RubyArgs forScript: aName withRubyArgs: rubyArgs withScriptArgs: scriptArgs .
   args processArgsInto: self env: envId .
        
   "Run either the file or the -e '...' scriptlets, but not both"
   retValue := args scriptlets ifEmpty:[
      (aName ~~ nil and: [ aName size ~~ 0 ]) ifTrue:[ |file|
        "Create the file, but do not look up file on $LOAD_PATH"
        file := RubyFile withGivenPath: aName fullPath: (RubyFile absolutePathFor: aName) .
        file ifNil: [ RubyLoadError signal: 'no such file to load -- ', aName ] .
        "Do NOT call loadFile or requireFile, as they look on the load path."
         retValue := self withFile: file do: [:f| f loadIntoEnv: envId ] ]
   ] ifNotEmpty: [ retValue := self evalDashEStrings: (args scriptlets) ] .
   commitFlag ifTrue: [ self commitTransaction ].
   ^ retValue .

