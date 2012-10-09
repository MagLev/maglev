*maglev-runtime
method: aGsNMethod env: envId selPrefix: rubyName
  "argument should usually be the method or bridge method
    with selector suffix  '#0*&'  "
      "ruby_selector_suffix dependent"
  aGsNMethod isRubyBridgeMethod ifTrue:[
    NameError signal:'expected a non-bridge method for ',  aGsNMethod selector .
  ].
  gsmeth := aGsNMethod .
  arity := aGsNMethod rubyArity .
  rubyName _isSymbol ifTrue:[
    selPrefix := rubyName asString immediateInvariant
  ] ifFalse:[
    rubyName isInvariant ifTrue:[ selPrefix := rubyName ]
          ifFalse:[ selPrefix := rubyName copy immediateInvariant ]
  ].

