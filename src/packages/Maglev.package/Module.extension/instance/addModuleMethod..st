*maglev-runtime
addModuleMethod: aSelector
  "a ruby primitive.
   Implement the Ruby instance method   module_function   for Module "

  ^ self addModuleMethod: aSelector env: 1"__callerEnvId" 

