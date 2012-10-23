*maglev-runtime
addModuleMethodIfEnabled: aSelector env: envId
  (self allModuleMethodsEnabled: envId) ifTrue:[ | theMod cm |
    theMod := self moduleMethodsModule .
    cm := self compiledMethodAt: aSelector rubyEnv: envId .
    cm ifNil:[ NameError signal:'no such method ', aSelector ].
    cm _isSmallInteger ifTrue:[
      NameError signal:'method ', aSelector , ' was undef-ed or protection overridden'
    ].
    theMod  addRubySelector: aSelector method: cm env: envId.
  ].

