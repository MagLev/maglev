*maglev-runtime
installPrimitive: rubySymbol selector: selString
  "a ruby primitive"
  ^ self installPrimitive: rubySymbol selector: selString withBridges: true 
                env: 1"__callerEnvId"

