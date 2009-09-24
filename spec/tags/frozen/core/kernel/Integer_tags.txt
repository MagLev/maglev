fails:Kernel.Integer uncritically return the value of to_int even if it is not an Integer
fails:Kernel.Integer returns the value of to_int if the result is a Bignum
fails:Kernel.Integer raises a FloatDomainError when passed NaN
fails:Kernel.Integer raises a FloatDomainError when passed Infinity
fails:Kernel.Integer raises an ArgumentError if the String contains a null byte
fails:Kernel#Integer uncritically return the value of to_int even if it is not an Integer
fails:Kernel#Integer returns the value of to_int if the result is a Bignum
fails:Kernel#Integer raises a FloatDomainError when passed NaN
fails:Kernel#Integer raises a FloatDomainError when passed Infinity
fails:Kernel#Integer raises an ArgumentError if the String contains a null byte
