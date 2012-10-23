*maglev-runtime
_rubyIncludeModule: aModule envId: envId
    | vCls pm |
    vCls := self _addRubyVirtualSuperclass: aModule kind: OM_RUBY_INCLUDE_Module
                envId: envId  .
    vCls transientRubySuperclass: envId put: (self rubySuperclass: envId) .
    pm := RubyCompilerState current persistenceMode .
    (pm and:[ self _persistable]) ifTrue:[
      vCls persistentRubySuperclass: envId put: (self persistentRubySuperclass: envId) .
      self persistentRubySuperclass: envId put: vCls .
    ].
    self transientRubySuperclass: envId put: vCls .
    self _clearLookupCaches: envId .
    ^ vCls

