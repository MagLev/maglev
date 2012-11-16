*maglev-runtime
installPrimitive: rubySymbol
  "a ruby primitive"
    ^ self installPrimitive: rubySymbol selector: rubySymbol withBridges: true 
                env: 1"__callerEnvId"

