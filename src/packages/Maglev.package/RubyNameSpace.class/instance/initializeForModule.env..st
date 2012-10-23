*maglev-runtime
initializeForModule: aModule   env: envArg
  envId := envArg .
  methodProtection := 0 .
  myClass := aModule .
  (aModule == Object and:[ parent ~~ nil ]) ifTrue:[
      self error:'Object''s name space parent non-nil'
  ].
  name := aModule rubyNameForModuleInit .
  moduleFrozen := false .
  "parent left as nil"
  "caller responsible for parent "

