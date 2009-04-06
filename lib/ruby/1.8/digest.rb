module Digest

  # call-seq:
  #     Digest.hexencode(string) -> hexencoded_string
  #
  # Generates a hex-encoded version of a given _string_.
  def hexencode(value)
    case value
    when Bignum
      value.to_s(16).downcase
    else
      raise ArgumentError, "Class #{value.class} not handled yet"
    end
  end

  module_function :hexencode

  def const_missing(c)
    # Recognize :SHA256, :SHA384, :SHA512 and load up the right libs
    # then return Digest.const_get(c)
    raise NotImplementedError
  end

  # This module provides instance methods for a digest implementation
  # object to calculate message digest values.
  module Instance
    # Instance methods that *should* be overridden
    def update(arg)
      raise NotImplementedError
    end
    def <<(arg)
      raise NotImplementedError
    end
    def finish
      raise NotImplementedError
    end
    def reset
      raise NotImplementedError
    end
    def digest_length
      raise NotImplementedError
    end
    def block_length
      raise NotImplementedError
    end

    # Instance methods that may be overridden
    def ==(other)
      raise NotImplementedError
    end
    def inspect
      raise NotImplementedError
    end

    # Instance methods that are not normally overridden
    def new
      raise NotImplementedError
    end
    def digest(*args)
      raise NotImplementedError
    end
    def digest!
      raise NotImplementedError
    end

    def hexdigest(*args)
      raise NotImplementedError
    end
    def hexdigest!
      raise NotImplementedError
    end
    def to_s
      raise NotImplementedError
    end
    def length
      raise NotImplementedError
    end
    def size
      raise NotImplementedError
    end
  end

  # This module stands as a base class for digest implementation classes.
  class Class
    include Instance

    # Returns the hash value of a given string. This is equivalent to
    # Digest::Class.new(*parameters).digest(string), where extra
    # parameters, if any, are passed through to the constructor and the
    # string is passed to digest().
    def self.digest(string, *params)
      raise NotImplementedError
    end

    # call-seq:
    #     Digest::Class.hexdigest(string[, ...]) -> hash_string
    #
    # Returns the hex-encoded hash value of a given _string_.  This is
    # almost equivalent to
    # Digest.hexencode(Digest::Class.new(*parameters).digest(string)).
    def self.hexdigest(string, *rest)
      puts "=== #{self}.hexdigest(#{string}, #{rest.inspect})"
      d = new(*rest)
      sum = d.digest(string)
      Digest.hexencode(sum)
    end

    # creates a digest object and reads a given file, name.
    # E.g.,
    #   p Digest::SHA256.file("X11R6.8.2-src.tar.bz2").hexdigest
    #   => "f02e3c85572dc9ad7cb77c2a638e3be24cc1b5bea9fdbb0b0299c9668475c534"
    #
    def self.file(name)
      raise NotImplementedError
    end
  end

  # This abstract class provides a common interface to message digest
  # implementation classes written in C.
  class Base < ::Digest::Class
    def initialize_copy(arg)
      raise NotImplementedError
    end
    def reset
      raise NotImplementedError
    end
    def update(arg)
      raise NotImplementedError
    end
    def <<(arg)
      raise NotImplementedError
    end
    def digest_length
      raise NotImplementedError
    end
    def block_length
      raise NotImplementedError
    end

    private
    def finish
      raise NotImplementedError
    end
  end
end
