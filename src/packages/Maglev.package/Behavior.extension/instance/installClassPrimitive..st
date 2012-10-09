*maglev-runtime
installClassPrimitive: rubySymbol
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: rubySymbol withBridges: true 
                env: 1"__callerEnvId"

