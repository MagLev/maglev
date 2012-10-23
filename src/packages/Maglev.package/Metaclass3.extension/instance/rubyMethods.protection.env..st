*maglev-runtime
rubyMethods: includeSuper protection: protInt env: envId
  "Return an IdentitySet of Symbols "
| set curClass  hidden  |
  set := IdentitySet new . hidden := IdentitySet new .
  curClass := self .
  [ curClass ~~ nil ] whileTrue: [
    curClass nonBridgeRubySelectorsInto: set hiddenInto: hidden protection: protInt env: envId .
    includeSuper ifTrue:[ curClass := curClass rubySuperclass: envId  ]
                ifFalse:[ curClass := nil "terminate loop" ].
  ].
  ^  set

