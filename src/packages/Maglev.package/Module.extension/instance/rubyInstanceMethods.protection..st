*maglev-runtime
rubyInstanceMethods: includeSuper protection: protInt
  "a ruby primitive"
  ^ self rubyMethods: includeSuper protection: protInt env: 1"__callerEnvId"

