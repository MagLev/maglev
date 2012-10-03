*maglev-runtime
addModuleMethodIfEnabled: aSelector env: envId
  self isMeta ifTrue:[ ^ self "do nothing" ].  
  ^ super addModuleMethodIfEnabled: aSelector env: envId 

