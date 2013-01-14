*maglev-runtime
_rubyModuleIncludeSelfEnv: envId
  "include a Module into a Module to provide the methodDict for module_methods"
  | vCls modCls pm tsuper psuper nam |
"nam := self name . "
  vCls := self _addRubyVirtualSuperclass: self kind: OM_MODULE_INCLUDE_self 
    envId: envId .  
  envId ~~ 1 ifTrue:[ self error:'envId > 1 not implemented']. "for name spaces"
  modCls := self virtualClass .
  tsuper := modCls rubySuperclass: envId .
  psuper := modCls persistentRubySuperclass: envId .
  "vCls transientNameSpace left as nil  "
  pm := RubyCompilerState current persistenceMode .
  (pm and:[ self _persistable]) ifTrue:[
    "vCls persistentNameSpace left as nil"
    vCls persistentRubySuperclass: envId put: psuper .
    modCls persistentRubySuperclass:  envId put:vCls .
  ].
  vCls transientRubySuperclass: envId put: tsuper .
  modCls transientRubySuperclass: envId put: vCls .
  self _clearLookupCaches: envId  .  "takes care of modCls also"
"GsFile gciLogServer:'IncludeModuleSelf for ' , nam . "
"nam == #Type ifTrue:[ self pause ]. "
  ^ vCls

