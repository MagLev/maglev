*maglev-runtime
initTopNameSpace: envId reload: reloadBool
  "used for bootstrap load of primitives only"
  | stdPath maglevHome tns |
  reloadBool ifFalse:[ self initModifiedStClasses: envId ].

  stdPath := self defaultLoadPath . 
  maglevHome := System gemEnvironmentVariable: 'MAGLEV_HOME' .
  maglevHome ifNil:[ self error:'MAGLEV_HOME not defined' ].
  stdPath insert: { (maglevHome, '/src/') } at: 1 .

  (RubyCompilerState initialize: envId) persistenceMode: true .
  RubyGlobalVarNode initialize: envId .
  reloadBool ifTrue:[ 
    "Object deleteMethods: 1 .  DONE IN reloadprims topaz script"
    tns := Object transientNameSpaceForStore: envId .
    (tns resolveConstant: #RUBY) _valueNoAction == self ifFalse:[
       Error signal: 'Object::RUBY not identical to RubyContext '
    ].
  ] ifFalse:[ 
    tns := RubyNameSpace initTopScope: envId  .
    tns at: #RUBY runtimePut: self .
  ].
  tns rubyGlobalVar: #'$:'  put:  stdPath ;
      rubyAlias: #'$LOAD_PATH' from:  #'$:' ;
      rubyAlias: #'$-I'  from:  #'$:' .

  reloadBool ifFalse:[
    tns rubyGlobalVar: #'$"' put: { } ;
         rubyAlias: #'$LOADED_FEATURES' from: #'$"' .
    (Object persistentNameSpace: envId) 
        rubyGlobalVar: #'$"' put: { } ;
        rubyAlias: #'$LOADED_FEATURES' from: #'$"' .
  ].
  self installPrimitiveBootstrap: envId .
  ^ self

