*maglev-runtime
_requirePrimitives_traceGlobals: gInt traceLocals: locInt env: envId reload: reloadBool 
   | res tns cst opns hm pfeatures |
   cst := RubyCompilerState current .
   cst persistenceMode: true .
   tns := Object transientNameSpaceForStore: envId .
   tns rubyGlobalVar: #'$MaglevInBootstrap' put: true . "allow ruby code to test $MaglevInBootstrap for debug"
   reloadBool ifTrue:[
     opns := Object persistentNameSpace: envId .
     pfeatures := (opns rubyGlobalVar: #'$"') copy .
   ] ifFalse:[
     pfeatures := { } .
   ].
   [  cst installingPrims: true ; reloadingPrims: reloadBool .
      envId == 1 ifTrue:[ 
        self deleteSmalltalkWrapperFiles ; copyRubyCHeaderFiles .
      ].
      res := self fileNamed: 'kernel/kernel.rb' env: envId do:[ :file |
      [ RubyParserM initializeParser . 
        RubyNameSpace  traceGlobals: gInt .
        RubyStaticScope traceLocals: locInt . 
        self class commitTransaction . "save class var changes"
        file loadIntoEnv: envId .
        self class commitTransaction . "save prims code"  
      ] ensure:[
        RubyNameSpace  traceGlobals: 0 .
        RubyStaticScope traceLocals: 0 .
      ].
     ].
   ] ensure:[
     cst installingPrims: false 
   ].
   self requireFileNamed: 'kernel/post_prims.rb' env: envId . "will abort due to new MainSelf"
     "kernel.rb and post_prims.rb use require to load the files.  This leaves $LOADED_FEATURES
      with all of the bootstrap files listed.  Now reset LOADED_FEATURES back 
      to previous state. "
   ( tns associationAt: #'$"' ) globalVarValue: pfeatures  .
   opns ifNil:[ opns := Object persistentNameSpace: envId ].
   ( opns associationAt: #'$"' ) globalVarValue: pfeatures .
   #( #TRUE #FALSE #NIL) do:[:sym | (opns associationAt: sym) immediateInvariant ].
   self _fixSuperclasses: envId .  "Object<-Symbol, ProtoObject<-Object"
   hm := System gemEnvironmentVariable: 'MAGLEV_HOME' .
   reloadBool ifFalse:[
     opns rubyGlobalVar: #'$:' put:  (RubyContext copyPath: (self defaultLoadPath) replacing: hm with: '$MAGLEV_HOME').
   ].
   tns rubyGlobalVar: #'$:' put:  self defaultLoadPath .  "separate copy for tns"
   tns rubyGlobalVar: #'$MaglevInBootstrap' put: nil .
   cst persistenceMode: false . 
   ^ res

