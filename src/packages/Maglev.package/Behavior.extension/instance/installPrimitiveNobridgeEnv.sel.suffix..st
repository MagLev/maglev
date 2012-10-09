*maglev-runtime
installPrimitiveNobridgeEnv: rubySymbol sel: prefix suffix: suffix
  | envId sel |
  envId := 1"__callerEnvId" .
  sel := prefix , envId asString , suffix .
  ^ self installPrimitive: rubySymbol selector: sel withBridges: false env: envId stEnv: envId

