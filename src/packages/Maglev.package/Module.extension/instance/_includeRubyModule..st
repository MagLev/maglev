*maglev-runtime
_includeRubyModule: aModule 
  "a ruby primitive"

  ^ self addRubyVirtualSuperclass: aModule forMeta: false 
           env: 1"__callerEnvId" .

