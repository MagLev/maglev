class Bignum
  # Bignum is identically  Smalltalk LargeInteger

  def self.name
    # override Smalltalk name
    'Bignum'
  end

  # methods from Number or Integer reimplemented in LargeInteger,
  #   and thus needing a primitive mapping here
  primitive 'abs', 'abs'
  primitive 'floor', 'floor'

  primitive 'to_i', 'truncated'
  primitive 'to_int' , 'truncated'
  primitive 'truncate' , 'truncated'

  primitive 'round', 'rounded'

end
