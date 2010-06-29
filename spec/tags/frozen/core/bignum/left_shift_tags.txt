fails:Bignum#<< with n << m returns 0 when m < 0 and m is a Bignum
fails:Bignum#<< with n << m returns a Fixnum == fixnum_max() when (fixnum_max() * 2) << -1 and n > 0
fails:Bignum#<< with n << m returns a Fixnum == fixnum_min() when (fixnum_min() * 2) << -1 and n < 0
