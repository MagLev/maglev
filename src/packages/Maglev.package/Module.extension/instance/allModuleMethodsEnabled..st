*maglev-runtime
allModuleMethodsEnabled: envId
  | tns assoc |
  self isMetaModule ifFalse:[
    (tns := self transientNameSpace: envId) ifNotNil:[
      assoc := tns associationAt: #_module_methods_all otherwise: nil . 
      assoc ifNotNil:[ ^ assoc _value == true ].
    ].
  ].
  ^ false

