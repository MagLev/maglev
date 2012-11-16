*maglev-runtime
installPrimitiveNobridge: rubySymbol selector: selString
  "a ruby primitive"
  ^ self installPrimitive: rubySymbol selector: selString withBridges: false  
                env: 1"__callerEnvId"

