*maglev-runtime
_includedModule: aModule env: envId
  "returns a virtual class, or nil "  
  | aCls |
  aModule == self ifTrue:[ ^ true ].
  aCls := self rubySuperclass: envId .
  [ aCls ~~ nil ] whileTrue:[
    aCls rubyPrimaryCopy == aModule ifTrue:[
      ^ aCls
    ].
    aCls := aCls rubySuperclass: envId .
  ].
  ^ nil

