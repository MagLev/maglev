*maglev-runtime
_rubyConstants: aSet
  | names |
  names := aSet ifNil:[ IdentitySet new ].
  self associationsDo:[ :assoc |
       (assoc isDefined ~~ nil and:[ assoc class ~~ RubyClassVarAssociation]) ifTrue:[
             names add: assoc key
       ].
  ].
    ^ names .

