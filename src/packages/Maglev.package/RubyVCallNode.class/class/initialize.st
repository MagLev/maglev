parsetree
initialize
  | dict |
  (dict := IdentityKeyValueDictionary new)
    at: #raise put: RubyVCallRaiseNode ;
    at: #binding put: RubyVCallBindingNode ;
    at: #'block_given?' put: RubyVCallBlockGivenNode ;
    at: #__callee__ put: RubyVCallCalleeNode ;
    at: #__method__ put: RubyVCallCalleeNode .

   RubyAbstractCallNode evalSelectors do:[ :sym |
       dict at: sym put:  RubyVCallEvalNode 
   ].
   dict immediateInvariant. 
   SpecialRubySelectors := dict .

