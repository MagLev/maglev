*maglev-runtime
initNameSpacesForExtend: envId
  "create name spaces and method dicts for extending a module.
   Returns receiver's transient name space"
  | tns tmds ofs pm |
  ofs := envId*4 + 2 .
  tns := (tmds := transientMethDicts) atOrNil: ofs .
  tns ifNil:[ | pns mds |
    (mds := methDicts) _isArray ifTrue:[
      pns := mds atOrNil: ofs .
    ] ifFalse:[
      mds := { mds }. "convert from single env 0 dict"
      methDicts := mds .
    ].
    (pns == nil and:[ (pm := RubyCompilerState current persistenceMode)  ]) ifTrue:[
      "extending a class in persistent mode"
      pns := RubyNameSpace new initializeForModule: self  env: envId.
      mds _rubyAt: ofs - 1  put: pns .
      (mds atOrNil: 1) ifNotNil:[ "a class with existing smalltalk behavior"
        self isRubySingletonClass ifFalse:[
          RubyContext default trackRubyClass: self env: envId  . "for ruby context reset"
        ].
      ].
    ].
    tns := RubyTransientNameSpace new initializeForModule: self persistentCopy: pns env: envId. 
    tmds _rubyAt: ofs - 1 put: tns . 
    self isCommitted ifTrue:[ (SessionTemps current at: #RubyPinnedClasses) add: self ].
  ] ifNotNil:[
     "else class already created or extended from ruby"
  ].
  tns moduleFrozen ifTrue:[ ArgumentTypeError signal:'attempt to extend a frozen module/class'].
  ^ tns

