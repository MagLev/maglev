*maglev-runtime
isDefinedForResolve: aSymbol inClass: aClass env: envId 
  "returns a RubySymbolAssociation or nil "

  isLoaded == false ifTrue:[  "initiate autoload"
     " the result may be a different association from a parent name space"
     ^ self _assocFor: aSymbol inClass: aClass env: envId error: true 
  ].
  isDefined ifNotNil:[ ^ self ].
  ^ nil

