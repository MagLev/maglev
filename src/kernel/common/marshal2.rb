# file marshal2.rb 

class Object
  def to_marshal(ms, strip_ivars = false)
    out = ms.serialize_extended_object self
    out << Marshal::TYPE_OBJECT
    out << ms.serialize(self.class.name.to_sym)
    out << ms.serialize_instance_variables_suffix(self, true, strip_ivars)
  end
end

class Range
  def to_marshal(ms)
    super(ms, true)
  end
end

class NilClass
  def to_marshal(ms)
    Marshal::TYPE_NIL
  end
end

class TrueClass
  def to_marshal(ms)
    Marshal::TYPE_TRUE
  end
end

class FalseClass
  def to_marshal(ms)
    Marshal::TYPE_FALSE
  end
end

class Class
  def to_marshal(ms)
    raise TypeError, "can't dump anonymous class #{self}" if self.name == ''
    Marshal::TYPE_CLASS + ms.serialize_integer(name.length) + name
  end
end

class Module
  def to_marshal(ms)
    raise TypeError, "can't dump anonymous module #{self}" if self.name == ''
    Marshal::TYPE_MODULE + ms.serialize_integer(name.length) + name
  end
end

class Symbol
  def to_marshal(ms)
    if idx = ms.find_symlink(self) then
      Marshal::TYPE_SYMLINK + ms.serialize_integer(idx)
    else
      ms.add_symlink self

      str = to_s
      Marshal::TYPE_SYMBOL + ms.serialize_integer(str.length) + str
    end
  end
end

class String
  def to_marshal(ms)
    out = ms.serialize_instance_variables_prefix(self)
    out << ms.serialize_extended_object(self)
    out << ms.serialize_user_class(self, String)
    out << Marshal::TYPE_STRING
    out << ms.serialize_integer(self.length) << self
    out << ms.serialize_instance_variables_suffix(self)
  end
end

class Fixnum
  def to_marshal(ms)
    Marshal::TYPE_FIXNUM + ms.serialize_integer(self)
  end
end

class Bignum
  def to_marshal(ms)
    str = Marshal::TYPE_BIGNUM + (self < 0 ? '-' : '+')
    cnt = 0
    num = self.abs

    while num != 0
      str << ms.to_byte(num)
      num >>= 8
      cnt += 1
    end

    if cnt % 2 == 1
      str << "\0"
      cnt += 1
    end

    str[0..1] + ms.serialize_integer(cnt / 2) + str[2..-1]
  end
end

class Regexp
  def to_marshal(ms)
    str = self.source
    out = ms.serialize_instance_variables_prefix(self)
    out << ms.serialize_extended_object(self)
    out << ms.serialize_user_class(self, Regexp)
    out << Marshal::TYPE_REGEXP
    out << ms.serialize_integer(str.length) + str
    out << ms.to_byte(options & 0x7)
    out << ms.serialize_instance_variables_suffix(self)
  end
end

class Struct
  def to_marshal(ms)
    out =  ms.serialize_instance_variables_prefix(self)
    out << ms.serialize_extended_object(self)

    out << Marshal::TYPE_STRUCT

    out << ms.serialize(self.class.name.to_sym)
    out << ms.serialize_integer(self.length)

    self.each_pair do |name, value|
      out << ms.serialize(name)
      out << ms.serialize(value)
    end

    out << ms.serialize_instance_variables_suffix(self)
    out
  end
end

class Array
  def to_marshal(ms)
    out = ms.serialize_instance_variables_prefix(self)
    out << ms.serialize_extended_object(self)
    out << ms.serialize_user_class(self, Array)
    out << Marshal::TYPE_ARRAY
    out << ms.serialize_integer(self.length)
    # Gemstone, optimization to use while loop
    n = 0
    lim = self.length
    while n < lim
      out << ms.serialize(self[n])
      n = n + 1
    end # end Gemstone
    out << ms.serialize_instance_variables_suffix(self)
  end
end

class Hash
  def to_marshal(ms)
    raise TypeError, "can't dump hash with default proc" if default_proc
    out = ms.serialize_instance_variables_prefix(self)
    out << ms.serialize_extended_object(self)
    out << ms.serialize_user_class(self, Hash)
    out << (self.default ? Marshal::TYPE_HASH_DEF : Marshal::TYPE_HASH)
    out << ms.serialize_integer(length)
    unless empty? then
      each_pair do |(key, val)|
        out << ms.serialize(key)
        out << ms.serialize(val)
      end
    end
    out << (default ? ms.serialize(default) : '')
    out << ms.serialize_instance_variables_suffix(self)
  end
end

class Float
  def to_marshal(ms)
    str = if nan? then
            "nan"
          elsif zero? then
            (1.0 / self) < 0 ? '-0' : '0'
          elsif infinite? then
            self < 0 ? "-inf" : "inf"
          else
            "%.*g" % [17, self] + ms.serialize_float_thing(self)
          end
    Marshal::TYPE_FLOAT + ms.serialize_integer(str.length) + str
  end
end

