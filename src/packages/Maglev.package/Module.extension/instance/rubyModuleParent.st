*maglev-runtime
rubyModuleParent
  | envId |
  envId := 1"__callerEnvId" .
  (self == Object) ifTrue: [^ nil ].
  ^ (self transientNameSpaceForStore: envId) parent myClass
