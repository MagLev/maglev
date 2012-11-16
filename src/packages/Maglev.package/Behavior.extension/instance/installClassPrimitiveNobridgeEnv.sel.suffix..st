*maglev-runtime
installClassPrimitiveNobridgeEnv: rubySymbol sel: prefix suffix: suffix 
  | envId sel |
  envId := 1"__callerEnvId" .
  sel := prefix , envId asString , suffix .
  ^ self class installPrimitive: rubySymbol selector: sel withBridges: false
                env: envId stEnv: envId

