*maglev-runtime
loadIntoEnv: envId
  | oldFile fileStk tns |
  cLibrary ifNotNil:[ 
    cLibrary == true ifFalse:[
      RubyLoadError signal:'shared library ' , loadName , ' already loaded'.
    ].
    self loadCextension: envId
  ] ifNil:[
    oldFile := (tns := Object transientNameSpaceForStore: envId )  at: #'__FILE__' otherwise: nil .
    tns  at: #'__FILE__'  compilePut: givenPath.
    self loadSource.
    fileStk := (RubyCompilerState initialize: envId) fileStack .
    fileStk push: self .

    ^ [ | comp |
        traceLoad ifTrue: [ 
            GsFile gciLogServer: '-- RubyFile>>load  : loading ', (RubyFile pathForTrace: path) 
        ] .
        comp := RubyCompiler new .
        comp compileFileNamed: path loadName: loadName env: envId
      ] ensure:[
        fileStk pop: self .
        tns  at: #'__FILE__'  compilePut: oldFile.             
      ]
  ]

