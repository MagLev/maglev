*maglev-runtime
_includeRubyModule: aModule 
  "a ruby primitive"
  ^ self addRubyVirtualSuperclass: aModule forMeta: self isMeta env: 1"__callerEnvId" .

