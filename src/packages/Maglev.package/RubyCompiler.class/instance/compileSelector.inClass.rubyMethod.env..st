*maglev-runtime
compileSelector: selectorArg inClass: aClass rubyMethod: aNode    env: envId 
  "Result is selector prefix of method added, or nil if remplementation disallowed"
    | ir cm cst compStack isMethDef aSymbol fil pos selPrefix prot |
    currentClass := aClass .
    currClassOrModule ifNil:[ currClassOrModule := aClass ].
    leaves := IdentityKeyValueDictionary new.
    compStack := (cst := RubyCompilerState initialize: envId) compilerStack. 
    compStack push: self .
    [ ir := aNode irMethodNode: envId forClass: aClass .
      (aSymbol := selectorArg) ifNil:[ aSymbol := aNode methodSelector ]. 
      (self reimplementationAllowed: aSymbol node: aNode cst: cst ) ifFalse:[ 
         ^ nil 
      ].
    ] ensure: [ 
       compStack pop: self
    ].
    isMethDef := aNode isMethodDefinition .
    ir class: aClass  .
    "fileName and source should already exist in aNode from walk phase"
    isMethDef ifTrue:[  ir addMethodProtection: ( prot := aClass rubyMethodProtection: envId ) ].
    ir selector: aSymbol . "after position installed, for better error message"
    cm := self compiledMethodForIR: ir .
    isMethDef ifTrue:[  
      selPrefix := self installBridgeMethodsFor: aSymbol in: aClass  
            argsDescr: aNode argsNode argsDescrInt optArgs: cm rubyOptArgsBits  
            protection: cm rubyMethodProtection primKind: 0 env: envId 
    ] ifFalse: [
     selPrefix := aSymbol rubySelectorPrefixSymbol
    ].
    aClass addRubySelector: aSymbol method: cm env: envId  .
    ( aClass allModuleMethodsEnabled: envId) ifTrue:[ aClass addModuleMethod: aSymbol env: envId ].
    needsSuperEach ifTrue:[ | rseSel rseSrc rseMth |
      envId == 1 ifFalse:[ self error:'__superEach only supported in env 1 ' ].
      rseSel := #__superEach: .
      (aClass baseCompiledMethodAt: rseSel environmentId: envId) ifNil:[
         rseSrc := (Object baseCompiledMethodAt: rseSel environmentId: 0 ) sourceString.
         "compile copied smalltalk code in env 0, install in envId method dicts" 
         rseMth := aClass _primitiveCompileMethod: rseSrc 
             symbolList: (System myUserProfile symbolList) category: nil 
            oldLitVars: nil intoMethodDict: false intoCategories: nil intoPragmas: nil environmentId: 0 .
         rseMth _isArray ifTrue:[ self error:'compilation error during __superEach ' ].
         aClass addRubySelector: rseSel method: rseMth env: envId .
      ].
    ].
    ^ selPrefix  "selector prefix returned for use in method_added calls" 

