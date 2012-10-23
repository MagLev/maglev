as yet unclassified
initialize
 | dict |
  dict := IdentityKeyValueDictionary new .
    
  RubyAbstractCallNode evalSelectors do:[ :sym |  dict at: sym put:  RubyCallEvalNode  ].
  dict immediateInvariant. 
  SpecialRubySelectors := dict .

