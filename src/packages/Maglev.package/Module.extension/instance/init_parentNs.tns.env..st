*maglev-runtime
init_parentNs: parTns tns: tnsArg env: envId
  | pm cst tns  |
  pm := (cst := RubyCompilerState current) persistenceMode .
  self _setRubyModulePersistentBit: pm .

  (tns := tnsArg) ifNotNil:[
     self installTransientNameSpace: tns persistentMode: pm env: envId .
    (pm and:[ cst installingPrims ]) ifTrue:[
        tns copyAssociationsToPns "for constants created at compile time"
     ].
  ] ifNil:[
    tns := self initNameSpacesForExtend: envId . 
  ].
  tns parent: parTns "maybe nil" .
  
  "for MODULE_moduFcts , do not share name spaces"
  ^ self

