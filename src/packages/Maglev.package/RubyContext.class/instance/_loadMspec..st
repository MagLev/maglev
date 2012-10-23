*maglev-runtime
_loadMspec: envId
  "load the mspec.rb from submodule of Gemstone internal git repository
   for debugging a spec file from  topaz -l .
   Returns receiver. "
  | saveDir |
  saveDir := RubyDirectory _getwd .
  [  | hm saveDir status rns |
     hm := RubyEnv _getenv:'MAGLEV_HOME' .
     RubyNameSpace traceGlobals: 0 .  
     status := RubyDirectory _chdir: hm, '/spec/mspec/lib' .
     status == 0 ifFalse:[ self error:' chdir failed' ]. 
     rns := Object transientNameSpaceForStore: envId .
     rns rubyGlobalVar: #'$0' put: 'mspec.rb' .
     self requireFileNamed: 'mspec.rb' env: envId.
     
     rns at: #DEBUG_SPEC runtimePut:  false  ;  
         at: #DEBUG_SPEC_VERBOSE runtimePut: false .
  ] ensure:[
     RubyDirectory _chdir: saveDir
  ].

