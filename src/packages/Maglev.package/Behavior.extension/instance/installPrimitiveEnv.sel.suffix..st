*maglev-runtime
installPrimitiveEnv: rubySymbol sel: prefix suffix: suffix
  "a ruby primitive"
  | envId sel |
  envId := 1"__callerEnvId" .
  sel := prefix , envId asString , suffix . 
  ^ self installPrimitive: rubySymbol selector: sel withBridges: true env: envId stEnv: envId

