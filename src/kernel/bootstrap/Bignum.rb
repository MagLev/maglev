class Bignum
  # Bignum is identically  Smalltalk LargeInteger

  # methods from Number or Integer reimplemented in LargeInteger,
  #   and thus needing a primitive mapping here
  
  # abs inherited from Integer
  
  primitive 'floor', 'floor'

  primitive 'to_i', 'truncated'
  primitive 'to_int' , 'truncated'
  primitive 'truncate' , 'truncated'

  primitive 'round', 'rounded'

end
