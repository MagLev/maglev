*maglev-runtime
defineModuleNamed: aSymbol rubyMethod: aNode inScope: parentArg env: envId
  | aModu parNs parVal moduTns aParent |
  parNs := (aParent := parentArg) nameSpaceOrNil: envId  . 
  parNs ifNil:[
     aParent isBehavior ifFalse:[ aParent := aParent _singletonClassFor: envId ].
     parNs := aParent nameSpace: envId .
  ] ifNotNil:[
     aParent isBehavior ifFalse:[ aParent := parNs myClass ].
  ].
  parNs ifNotNil:[  "a singleton class may have no name space"
     parVal := parNs classOrNameSpaceAt: aSymbol inModule: aParent .
     parVal isBehavior ifTrue:[ aModu := parVal ]
                 ifFalse:[ parVal isNameSpace ifTrue:[ moduTns := parVal transientCopy ]].
  ].
  aModu ifNil:[ 
     aModu := Module _newModule .
     aModu name: aSymbol .
     parNs ifNil:[ parNs := aParent initNameSpacesForExtend: envId ]
           ifNotNil:[ parNs := parNs transientCopy ].
     aModu init_parentNs: parNs tns: moduTns env: envId .
     aModu _incrementCachedConstantsSerialNum .
     parNs at: aSymbol runtimePut: aModu .
  ] ifNotNil:[ | cst |
    aModu is_aModule ifFalse:[ ArgumentTypeError signal:'arg to module keyword exists but is not a Module'].
    RubyCompilerState current installingPrims ifTrue:[  "reloading bootstrap code"
      (aModu transientNameSpace: envId) ifNil:[   
        aModu deleteMethods: envId .  "remove all persistent methods"
        aModu moduleMethodsModule deleteMethods: envId .
        aModu transientNameSpaceForStore: envId . "create a tns"
      ].
    ]
  ].
  aNode ifNotNil:[
    self extendModule: aModu rubyMethod: aNode env: envId .
  ].
  ^ aModu

