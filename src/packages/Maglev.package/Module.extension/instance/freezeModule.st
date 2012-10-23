*maglev-runtime
freezeModule
  | envId | 
  envId := 1"__callerEnvId" .
  self _freezeModule: envId .
  self virtualClass _freezeModule: envId .

