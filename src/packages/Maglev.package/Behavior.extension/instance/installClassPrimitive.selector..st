*maglev-runtime
installClassPrimitive: rubySymbol selector: selString
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: selString withBridges: true 
                env: 1"__callerEnvId"

