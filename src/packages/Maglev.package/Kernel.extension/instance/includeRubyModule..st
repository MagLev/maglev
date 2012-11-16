*maglev-runtime
includeRubyModule: aModule
  "a ruby primitive.
   used by  include   at  ruby main program level, not within a class or module"
  self class == Object ifFalse:[ ArgumentTypeError signal:'invalid receiver for include'].
  ^ Object addRubyVirtualSuperclass: aModule forMeta: false env: 1"__callerEnvId"  

