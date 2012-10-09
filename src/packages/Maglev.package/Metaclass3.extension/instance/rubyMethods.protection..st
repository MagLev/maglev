*maglev-runtime
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  self isMeta ifTrue:[ ^ IdentitySet new ].
  ^ self class rubyMethods: includeSuper protection: protInt env: 1"__callerEnvId"

