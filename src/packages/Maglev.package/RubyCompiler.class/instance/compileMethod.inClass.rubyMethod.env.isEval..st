*maglev-runtime
compileMethod: selectorArg inClass: aClass rubyMethod: aNode env: envId isEval: evalBool
  "Result is a GsNMethod , result not added to any method dict"
    | ir cm cst compStack  aSymbol fil pos |
    "self startLog. "
    currentClass := aClass .
    currClassOrModule ifNil:[ currClassOrModule := aClass ].
    leaves := IdentityKeyValueDictionary new.
    compStack := (cst := RubyCompilerState initialize: envId) compilerStack. 
    compStack push: self .
    [ " GsFile gciLogServer:'compiling ' , aSymbol , ' in ' , aClass name  "
      ir := aNode irMethodNode: envId forClass: aClass .   
      evalBool ifTrue:[ ir setRubyEval ].
      (aSymbol := selectorArg) ifNil:[ aSymbol := aNode methodSelector ]. 
    ] ensure: [
       compStack pop: self
    ].
    ir class: currentClass .
   
    ir selector: aSymbol . "after position installed, for better error message"
    cm := self compiledMethodForIR: ir .
    ^ cm 

