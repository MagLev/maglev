*maglev-runtime
initialize
  | dict |
  dict := IdentityKeyValueDictionary new .
  #( #new #initialize #initialize_copy #coerce_to #coerce_to_or_nil )
      do:[ :sym |  dict at: sym put: 1 ].

  self evalSelectors do:[ :sym |    dict at: sym put:  2 ].
 
  dict at: #__fixed_instvars put: 3 .

  dict immediateInvariant.
  SpecialSelectorKinds := dict .

  " following selectors are base name, used before suffix for send is computed"
  RcvrNoToProcSelectors := IdentitySet withAll: #(
     #'_equal?'  #'_not_equal?'  #'_is_a?'  #'_kind_of?'  
     #_isInteger #_isSmallInteger #_isFixnum #_isNumber #_isNumeric
     #_isFloat #_isScaledDecimal #_isSymbol #_isExceptionClass #_isExecBlock
     #_isBlock #_isArray #_isOneByteString #_isStringOrSymbol #_isString
     #_isRubyString #_isRubyHash #_isHash #_isRegexp #_isRange 
     #arity  #call #inspect #'[]' ) .

  LastArgNoToProcSelectors := IdentitySet withAll: #(
     #'_equal?'  #'_not_equal?'  #'_is_a?'  #'_kind_of?' ).
         "assume #'call&'  handled by hasBlockArg logic"

