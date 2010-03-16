module Precision    # added for 1.8.7

  # def self.included ; end # not reimplemented yet

  def prec(klass)
    klass.induced_from(self)
  end
 
  def prec_f
    Float.induced_from(self)
  end
 
  def prec_i
    Integer.induced_from(self)
  end
end

class Float
  include Precision

  def prec(klass)
    if klass._equal?(Float)
      return self
    end
    klass.induced_from(self)
  end

  def prec_f
    self
  end

  def prec_i
    self.to_i
  end
end

class Integer
  include Precision

  def prec(klass)
    if klass._equal?(Integer)
      return self
    end
    klass.induced_from(self)
  end

  def prec_f
    self.__to_float
  end

  def prec_i
    self
  end
end

class Fixnum
  def prec(klass)
    if klass._equal?(Fixnum)
      return self
    end
    super(klass)
  end
end

class Bignum
  def prec(klass)
    if klass._equal?(Bignum)
      return self
    end
    super(klass)
  end
end


