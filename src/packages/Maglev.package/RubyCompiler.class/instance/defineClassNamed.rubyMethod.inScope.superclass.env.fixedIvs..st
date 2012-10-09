*maglev-runtime
defineClassNamed: aSymbol rubyMethod: aNode inScope: parentArg   
    superclass: superCls  env: envId fixedIvs: ivList
      "ruby_selector_suffix dependent, use of perform"
  |  aCls res parNs parVal clsTns inBoot aParent  |
  parNs := (aParent := parentArg) nameSpaceOrNil: envId  . 
  parNs ifNil:[
     aParent isBehavior ifFalse:[ aParent := aParent _singletonClassFor: envId ].
     parNs := aParent nameSpace: envId .
  ] ifNotNil:[
     aParent isBehavior ifFalse:[ aParent := parNs myClass ].
  ].
  parNs ifNotNil:[  "a singleton class may have no name space"
    parVal := parNs classOrNameSpaceAt: aSymbol inModule: aParent .
    parVal isBehavior ifTrue:[ aCls := parVal ]
                 ifFalse:[ parVal isNameSpace ifTrue:[ clsTns := parVal transientCopy ]].
  ].        
  inBoot := installingPrims .       
  aCls ifNil:[   | cst  |
    cst := RubyCompilerState current .
    aCls :=  superCls newRubySubclass: aSymbol instancesPersistent: cst persistableInstances
        fixedIvs: ivList .
    parNs ifNil:[ parNs := aParent initNameSpacesForExtend: envId ]
          ifNotNil:[ parNs := parNs transientCopy ].
    self initSubclass: aCls name: aSymbol parentNs: parNs tns: clsTns cState: cst env: envId. 
    aCls _incrementCachedConstantsSerialNum .
    parNs at: aSymbol runtimePut: aCls .
    inBoot ifFalse:[
      aCls immediateInvariant.
      "Can't use @ruby1, must use perform to bypass meth protection"
      superCls with: aCls perform:#'inherited#1__' env: 1 .
           "See also kernel/bootstrap/Class.rb for Class.new(superCls)"
      res := aNode ifNotNil:[ self extendClass: aCls rubyMethod: aNode env: envId ]
                   ifNil:[ aCls ].
    ] ifTrue:[   
      " send of #inherited not done in bootstrap"
      "leave class modifiable, for fixedInstvar to be defined automatically based
       on uses in method bodies."
      res := aNode ifNotNil:[ self extendClass: aCls rubyMethod: aNode env: envId ]
                   ifNil:[ aCls ].
      aCls immediateInvariant.
    ].
  ] ifNotNil:[ | existingSuper |
    ivList size ~~ 0 ifTrue:[ 
       ArgumentError signal:'__fixed_instvars usable only in first opening of a class'
    ].
    superCls == Object ifFalse:[  "deleted inBoot term"
      (existingSuper := aCls superClass "non-virt superclass") == superCls ifFalse:[
           ArgumentTypeError signal: 'superclass mismatch for ', aSymbol . 
      ].
    ].
    inBoot ifTrue:[ 
      (aCls transientNameSpace: envId) ifNil:[
        aCls deleteMethods: envId .  "remove all persistent methods"
        aCls virtualClass deleteMethods: envId .
      ].
    ].
    clsTns := aCls initNameSpacesForExtend: envId .  "creates a tns"
    clsTns _parent ifNil:[ aCls ~~ Object ifTrue:[ clsTns parent: parNs "extending a Smalltalk cass"]].
    inBoot ifTrue:[
      (aCls transientNameSpaceForStore: envId) _name: aSymbol .  "override smalltalk name"
    ].    
    res := aNode ifNotNil:[ self extendClass: aCls rubyMethod: aNode env: envId ]
        ifNil:[ aCls ].
  ].
  ^ res

