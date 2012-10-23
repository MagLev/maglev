*maglev-runtime
installClassPrimitiveNobridge: rubySymbol selector: selString
  "a ruby primitive"
  ^ self class installPrimitive: rubySymbol selector: selString withBridges: false  
                env: 1"__callerEnvId"

