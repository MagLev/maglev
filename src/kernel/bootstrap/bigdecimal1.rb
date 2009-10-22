#  copied from Rubinius approx March 09
#  changes:
#    use Maglev _is  methods instead of kind_of? where possible
#    use equal?(nil) instead of nil ? 
#
#  as of 7Oct09  Rubinius is using a C implementation .
#  This MagLev  implementation drastically rewritten.
#    It assumes an efficient underlying Bignum implementation,
#    and a C primitive in Integer to count the number of decimal digits.
#

class BigDecimal < Numeric
  # See stdlib/ext/bigdecimal for MatzRuby implementation.

  # for internal structure see comments under accessors in bigdecimal1.rb

  include Comparable

  #############
  # Constants #
  #############
  
  SIGN_POSITIVE_ZERO = 1
  SIGN_NEGATIVE_ZERO = -1
  SIGN_POSITIVE_FINITE = 2
  SIGN_NEGATIVE_FINITE = -2
  SIGN_POSITIVE_INFINITE = 3
  SIGN_NEGATIVE_INFINITE = -3
  SIGN_NaN = 0 
  
  # RADIX = '.'
  # EXP = 'E'
  # SIGNS = {-1 => MINUS, 0 => nil, 1 => PLUS} # for 

  TEN_POWER_TABLE = [ 0, 10, 100, 1000, 10_000, 100_000, 1000_000,
		     10_000_000, 100_000_000, 1000_000_000 ]
  #   this table stops at 10**9 because multiply of a Bignum times
  #   a Fixnum is more efficient if individual digit multiplies don't overflow
  #   a 64bit accumulator .

  BASE = 1000000000  # for MRI compatibility, not used internally

  # values for first arg to mode() methods
  EXCEPTION_NaN = :EXCEPTION_NaN
  EXCEPTION_INFINITY = :EXCEPTION_INFINITY
  EXCEPTION_UNDERFLOW = :EXCEPTION_UNDERFLOW
  EXCEPTION_OVERFLOW  = :EXCEPTION_OVERFLOW
  EXCEPTION_ZERODIVIDE = :EXCEPTION_ZERODIVIDE
  ROUND_MODE    = :ROUND_MODE

  RAISE_on_OVERF = true  # exponent overflow beyond Fixnum range raises,
			              # RAISE_on_OVERF  cannot be shut off

  # values for second arg to mode() used with ROUND_MODE
  ROUND_UP = 1   
  ROUND_DOWN = -1
  ROUND_HALF_UP = 0
  # ROUND_HALF_EVEN # not implemented yet
  # ROUND_HALF_DOWN # not implemented yet
  # ROUND_CEILING # not implemented yet
  # ROUND_FLOOR # not implemented yet
  
  # internal constant
  UNLIM_PRECISION = Fixnum::MAX
  
  def _init_nan  
    # define all the fixed instVars by implementing _init_nan
    #  reimplemented in bigdecimal.rb
    @special = 2
    @precs = 99 
    @sign = 1
    @digits = 0
    # @num_digits = 1
    @exp = 0
    self
  end

end
BigDecimal._freeze_constants

