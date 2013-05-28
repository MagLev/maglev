*maglev-runtime
rubyModuleParent
  | envId parent |
  envId := 1 "__callerEnvId" .
  parent := (self transientNameSpaceForStore: envId) parent.
  parent ifNil: [ ^ nil] ifNotNil: [ ^ parent myClass ].
