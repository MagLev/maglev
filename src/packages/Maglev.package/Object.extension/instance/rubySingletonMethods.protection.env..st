*maglev-runtime
rubySingletonMethods: includeModules protection: protInt env: envId
  "Return an IdentitySet of Symbols, the names of the singleton methods for receiver.
   If receiver has no singleton methods, return an empty array.
   If includeModules is true, then also include methods from mixed in
   modules."
  | curClass set hidden |
  set := IdentitySet new .  hidden := IdentitySet new .
  curClass := self virtualClass .

  "For a class, the singleton class is the meta class"
  self isBehavior ifTrue: [
    self theMetaClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId.
    includeModules ifTrue: [ | c |
      c := curClass .
      [ c ~~ nil and: [ c ~~ Object and: [ c ~~ Object theMetaClass ] ] ] whileTrue: [
      c nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId .
      c := c rubySuperclass: envId  .
      ] .
    ] .
  ] .

  [ curClass ~~ nil and: [ curClass isRubyVirtual ] ] whileTrue: [
    (curClass _isIncludableInSingletons: includeModules) ifTrue:[ 
	   curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt
	               env: envId 
	 ] .
     curClass := curClass rubySuperclass: envId .
  ] .
  ^ set 

