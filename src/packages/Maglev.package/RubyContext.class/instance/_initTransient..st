*maglev-runtime
_initTransient: envId
  "executed at session startup, and by smalltalk main programs which reinitialize
    transient state when running vmunit or rubyspec tests."
  | tns path argv  argf assoc |
  tns := Object transientNameSpaceForStore: envId .
  (assoc := tns rubyGlobalVar: #'$"' put:  (self persistentLoadedFeatures: envId) copy ) 
      setReadOnly .
  tns _rubyAlias: #'$LOADED_FEATURES' fromAssoc: assoc .

  (assoc := tns rubyGlobalVar: #'$:' put:  (path := self persistentLoadPathCopy: envId) ) 
    setReadOnly . 
  tns _rubyAlias: #'$LOAD_PATH' fromAssoc:  assoc ;
      _rubyAlias: #'$-I'        fromAssoc:  assoc .

  tns at: #MAGLEV_MARSHAL_CLASS_CACHE transientRuntimePut: RubyIdentityHash new ;
      at: #ARGV   transientRuntimePut: ( argv := { } ) ;
      at: #ARGF   transientRuntimePut: (argf := RubyArgf with: argv) ;
      at: #ENV       transientRuntimePut: (RubyEnv _currentEnvAssoc _value) .
      "RUBY_PLATFORM, PLATFORM from transient_const_set  in post_prims/Object.rb "
  (tns rubyGlobalVar: #'$*' put: argv) setReadOnly .
  (tns rubyGlobalVar: #'$<' put: argf) setReadOnly .
  (tns rubyGlobalVar: #'$-a' put: nil) setReadOnly .  "maglev-ruby does not implement -a"
  (tns rubyGlobalVar: #'$-l' put: nil) setReadOnly .  "maglev-ruby does not implement -l"
  (tns rubyGlobalVar: #'$-p' put: nil) setReadOnly .  "maglev-ruby does not implement -p"

  tns addTransientAssociation:( RubyDynGlobalVarAssociation newWithKey: #'$FILENAME'
                                            with:[ argf @ruby1:filename ] ).
  RubyGlobalVarNode sessionInitialize .
  "MAGLEV_SESSION_TEMPS not used anymore"
  "TOPLEVEL_BINDING  installed by generated code , see  #installTopBinding:  "
  envId == 1 ifTrue:[
    RubySocket _initTransientSocketConstants: envId . 
    self _initFcntlConstants: envId .
    self _initProcessConstants: envId .
    RubyThrowException _initSignalConstants: envId .
  ] ifFalse:[
    envId == 2 ifTrue:[ "RubyParser does not use Socket or Fcntl"]
              ifFalse:[ self error:'Socket,Fcntl init needs work for per-envId key in SessionTemps'].
  ].

