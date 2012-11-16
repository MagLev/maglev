*maglev-runtime
installClassPrimitiveNobridge: rubySymbol
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: rubySymbol withBridges: false 
         env: 1"__callerEnvId" 

