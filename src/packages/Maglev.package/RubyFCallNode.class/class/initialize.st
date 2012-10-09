*maglev-runtime
initialize
  | dict |
  (dict := IdentityKeyValueDictionary new)
    at: #'block_given?' put: RubyBlockGivenNode ;
    at: #binding        put: RubyFCallBindingNode ;
    at: #__callee__     put: RubyFCallCalleeNode ;
    at: #__method__     put: RubyFCallCalleeNode .

   RubyAbstractCallNode evalSelectors do:[ :sym| dict at: sym put: RubyFCallEvalNode ].
   dict immediateInvariant. 
   SpecialRubySelectors := dict .

