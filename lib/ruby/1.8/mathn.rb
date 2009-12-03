#
#   mathn.rb - 
#   	$Release Version: 0.5 $
#   	$Revision: 1.1.1.1.4.1 $
#   	$Date: 1998/01/16 12:36:05 $
#   	by Keiju ISHITSUKA(SHL Japan Inc.)
#
# --
#
#   
#

require "complex.rb"
require "rational.rb"
require "matrix.rb"

class Integer

  def gcd2(int)
    a = self.abs
    b = int.abs
    # a, b = b, a if a < b
    if a < b
      tmp = a
      a = b
      b = tmp
    end
    
    pd_a = a.prime_division
    pd_b = b.prime_division
    
    gcd = 1

    # for pair in pd_a
    pd_a_len = pd_a.size
    pd_a_idx = 0
    while pd_a_idx < pd_a_len
      pair = pd_a[pd_a_idx]
      pd_a_idx += 1

      as = pd_b.assoc(pair[0])
      if as
	gcd *= as[0] ** ( as[1].__min( pair[1]) )
      end
    end
    return gcd
  end
  
  def Integer.from_prime_division(pd)
    value = 1

    # for prime, index in pd
    pd_len = pd.size
    pd_idx = 0
    while pd_idx < pd_len
      pair = pd[pd_idx]
      pd_idx += 1
      prime = pair[0]
      index = pair[1]

      value *= prime**index
    end
    value
  end
  
  def prime_division
    raise ZeroDivisionError if self == 0
    ps = Prime.new
    value = self
    pv = []
    for prime in ps
      count = 0
      while (value1, mod = value.divmod(prime)
	     mod) == 0
	value = value1
	count += 1
      end
      if count != 0
	pv.push [prime, count]
      end
      break if prime * prime  >= value
    end
    if value > 1
      pv.push [value, 1]
    end
    return pv
  end
end
  
class Prime
  include Enumerable

  def initialize
    @seed = 1
    @primes = []
    @counts = []
  end
  
  def succ
    i = -1
    lprimes = @primes
    size = lprimes.size
    lcounts = @counts
    lseed = @seed
    while i < size
      if i == -1 
        lseed = lseed + 1
        i += 1
      else
        lcounts_i = lcounts[i]
        if (lseed > lcounts_i) 
          lprimes_i = lprimes[i]
	  lcounts_i += lprimes_i
          while lseed > lcounts_i
            lcounts_i += lprimes_i
          end
          lcounts[i] = lcounts_i
        end
        if lseed != lcounts_i
	  i += 1
        else
	  i = -1
        end
      end
    end
    lprimes.push( lseed)
    lcounts.push( lseed + lseed )
    @seed = lseed
    return lseed
  end

  alias next succ

  def each
    loop do
      yield succ
    end
  end
end

class Fixnum
  alias / quo
end

class Bignum
  alias / quo
end

class Rational
  Unify = true

  def inspect
    format "%s/%s", numerator.inspect, denominator.inspect
  end

  alias power! **

  def ** (other)
    if other._kind_of?(Rational)
      other2 = other
      if (self <=> 0) < 0
	return Complex.new!(self, 0) ** other
      elsif other == 0
	return Rational(1,1)
      elsif self == 0
	return Rational(0,1)
      elsif self == 1
	return Rational(1,1)
      end
      
      npd = numerator.prime_division
      dpd = denominator.prime_division
      if (other <=> 0) < 0
	other = -other
	npd, dpd = dpd, npd
      end
      
      # for elm in npd
      npd_len = npd.size
      npd_idx = 0
      while npd_idx < npd_len 
        elm = npd[npd_idx]
        npd_idx += 1

	elm[1] = elm[1] * other
	if ! elm[1]._isInteger and elm[1].denominator != 1
         return Float(self) ** other2
	end
	elm[1] = elm[1].to_i
      end
      
      # for elm in dpd
      dpd_len = dpd.size
      dpd_idx = 0
      while dpd_idx < dpd_len 
        elm = dpd[dpd_idx]
        dpd_idx += 1

	elm[1] = elm[1] * other
	if !elm[1]._isInteger and elm[1].denominator != 1
         return Float(self) ** other2
	end
	elm[1] = elm[1].to_i
      end
      
      num = Integer.from_prime_division(npd)
      den = Integer.from_prime_division(dpd)
      
      Rational(num,den)
      
    elsif other._isInteger
      if other > 0
	num = numerator ** other
	den = denominator ** other
      elsif other < 0
	num = denominator ** -other
	den = numerator ** -other
      elsif other == 0
	num = 1
	den = 1
      end
      Rational.new!(num, den)
    elsif other._isFloat
      Float(self) ** other
    else
      x , y = other.coerce(self)
      x ** y
    end
  end

  def power2(other)
    if other._kind_of?(Rational)
      if (self <=> 0) < 0
	return Complex(self, 0) ** other
      elsif other == 0
	return Rational(1,1)
      elsif self == 0
	return Rational(0,1)
      elsif self == 1
	return Rational(1,1)
      end
      
      dem = nil
      x = self.denominator.to_f.to_i
      neard = self.denominator.to_f ** (1.0/other.denominator.to_f)
      loop do
	if (neard**other.denominator == self.denominator)
	  dem = neaed
	  break
	end
      end
      nearn = self.numerator.to_f ** (1.0/other.denominator.to_f)
      Rational(num,den)
      
    elsif other._isInteger
      if other > 0
	num = numerator ** other
	den = denominator ** other
      elsif other < 0
	num = denominator ** -other
	den = numerator ** -other
      elsif other == 0
	num = 1
	den = 1
      end
      Rational.new!(num, den)
    elsif other._isFloat
      Float(self) ** other
    else
      x , y = other.coerce(self)
      x ** y
    end
  end
end

module Math
  def sqrt(a)
    if a._kind_of?(Complex)
      abs = sqrt(a.real*a.real + a.image*a.image)
#      if not abs.kind_of?(Rational)
#	return a**Rational(1,2)
#      end
      x = sqrt((a.real + abs)/Rational(2))
      y = sqrt((-a.real + abs)/Rational(2))
#      if !(x.kind_of?(Rational) and y.kind_of?(Rational))
#	return a**Rational(1,2)
#      end
      if (a.image <=> 0) >= 0 
	Complex(x, y)
      else
	Complex(x, -y)
      end
    elsif (a <=> 0) >= 0
      rsqrt(a)
    else
      Complex(0,rsqrt(-a))
    end
  end
  
  def rsqrt(a)
    if a._isFloat
      sqrt!(a)
    elsif a._kind_of?(Rational)
      rsqrt(a.numerator)/rsqrt(a.denominator)
    else
      src = a
      max = 2 ** 32
      byte_a = [src & 0xffffffff]
      # ruby's bug
      while (src >= max) and (src >>= 32)
	byte_a.unshift src & 0xffffffff
      end
      
      answer = 0
      main = 0
      side = 0
      for elm in byte_a
	main = (main << 32) + elm
	side <<= 16
	if answer != 0
	  if main * 4  < side * side
	    applo = main.div(side)
	  else 
	    applo = ((sqrt!(side * side + 4 * main) - side)/2.0).to_i + 1
	  end
	else
	  applo = sqrt!(main).to_i + 1
	end
	
	while (x = (side + applo) * applo) > main
	  applo -= 1
	end
	main -= x
	answer = (answer << 16) + applo
	side += applo * 2
      end
      if main == 0
	answer
      else
	sqrt!(a)
      end
    end
  end

  module_function :sqrt
  module_function :rsqrt
end

class Complex
  Unify = true
end

