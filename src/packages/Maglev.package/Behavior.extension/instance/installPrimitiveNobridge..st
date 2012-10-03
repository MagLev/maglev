*maglev-runtime
installPrimitiveNobridge: rubySymbol
  "a ruby primitive"
    ^ self installPrimitive: rubySymbol selector: rubySymbol withBridges: false 
                env: 1"__callerEnvId"

