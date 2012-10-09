*maglev-runtime
rubyMethods: includeSuper protection: protInt
  "a ruby primitive"
  ^ self virtualClass rubyMethods: includeSuper protection: protInt 
            env: 1"__callerEnvId"

