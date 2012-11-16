as yet unclassified
_rubyConstantsFreeze
  | pns |
  super _rubyConstantsFreeze .
  ((pns := persistCopy) ~~ nil and:[RubyCompilerState current persistenceMode]) ifTrue:[
    pns _rubyConstantsFreeze .
  ].

