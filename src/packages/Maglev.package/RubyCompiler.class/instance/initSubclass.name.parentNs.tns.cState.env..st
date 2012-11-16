*maglev-runtime
initSubclass: cls name: aSymbol parentNs: parTns tns: tnsArg cState: cst env: envId
  | pm cx tns metaCls  |
  pm := cst persistenceMode .
  cls _setRubyModulePersistentBit: pm .
  " meta _setInstancesPersistentBit:   done by   newRubySubclass:... "
  
  (tns := tnsArg) ifNotNil:[
	 cls installTransientNameSpace: tns persistentMode: pm env: envId .
	 (pm and:[ cst installingPrims ]) ifTrue:[
		 tns copyAssociationsToPns "for constants created at compile time"
	 ].
  ] ifNil:[
    tns := cls initNameSpacesForExtend: envId . 
  ].
  tns parent: parTns "maybe nil" .

  tns := (metaCls := cls virtualClass) initNameSpacesForExtend: envId .
  tns parent: (parTns myClass virtualClass transientNameSpaceForStore: envId) .

