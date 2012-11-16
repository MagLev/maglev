as yet unclassified
initModifiedStClasses: envId 
  | arr set dict |
  (arr := modifiedStClasses ) ifNil:[
	 arr := Array new: envId .
	 modifiedStClasses := arr .
  ] ifNotNil:[
     arr size < envId ifTrue:[ arr size: envId ].
  ].
  (set := IdentitySet new ) add: Object .
  arr at: envId put: set .
  "----------------"
  (arr := rubyPrimMethods) ifNil:[
	  arr := Array new: envId .
	  rubyPrimMethods := arr .
  ] ifNotNil:[
     arr size < envId ifTrue:[ arr size: envId ].
  ].
  dict := IdentityKeyValueDictionary new .  "keys are GsNMethods, values are Symbols or Array of Symbols"
  arr at: envId put: dict  .

