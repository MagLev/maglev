*maglev-runtime
isModifiable_oops_notModule
  | ivn |
  self isInvariant ifTrue:[ ^ false ].

  ((ivn := instVarNames) ~~ #() and:[ ivn isInvariant]) ifTrue:[ ^ false ].

  ^ (format bitAnd: 1) == 0 "neither bytes nor special"     

