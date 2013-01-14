*maglev-runtime
initTransient: envId
  "executed at session startup, when instance created for bootstrap 
     or when instance loaded from committed state"
" | thrEnv |
  thrEnv := self __threadRubyEnvId  .
  [ 
"
    | tns f |
    "self __threadRubyEnvId: envId." "So RubyEnv and RubyHash instances work"
    RubyCompilerState initialize: envId .
    tns := Object transientNameSpaceForStore: envId .
    "assume maglev-ruby script always using topaz -l ; GsFile server/client are same"
    tns  at: #STDIN  transientRuntimePut: (f := GsFile _stdinServer ) ;
         rubyGlobalVar: #'$stdin'  put: f ;
         at: #STDOUT transientRuntimePut: (f := GsFile _stdoutServer ) .
    (tns rubyGlobalVar: #'$stdout' put: f ) "for 1.9 only  setReadOnly"  .
    tns  rubyGlobalVar: #'$>'      put: f ;
         at: #STDERR transientRuntimePut: (f := GsFile _stderrServer );
         rubyGlobalVar: #'$stderr' put: f .
    (tns rubyGlobalVar: #'$$'      put: System _gemProcessId ) setReadOnly .
    (tns rubyGlobalVar: #'$SAFE' put:  0 ) setReadOnly .
   (tns associationAt:#'$SAFE') immediateInvariant .   
"above list should match code in transientAssocsForReinit"
     self _initTransient: envId .
" ] ensure:[
    self __threadRubyEnvId: thrEnv
  ]
"

