#--
# Since the MagLev VM ships with a copy of openssl, we use that
# implementation of message digests for both the OpenSSL library and the
# Digest Library.
#++

#
# The Digest module defines a common API for various hashing algorithms
# (MD5, SHA1, etc.).  The methods provide an algorithm agnostic API for
# dealing with the digests.  Each implementation of a Digest algorithm
# implements conforms to the API defined in Digest::Instance.  The most
# used methods include:
#
#   reset()         # Reset state
#   finish()        # finalize state, calculate and return the hash
#   update(str)     # Add more data to the state
#   <<(str)         # alias :<< :update
#
# MagLev supports the following digests: MD5, SHA1.
#
# == Examples
#
#   require 'digest'
#   Digest::MD5.hexdigest("my data")  # => "1291e1c0aa879147f51f4a279e7c2e55"
#
# You can also incrementally add data to a digest (e.g., reading a stream
# from IO):
#
#   require 'digest'
#   sha1 = Digest::SHA1.new
#   sha1 << "some data"
#   sha1 << "more data"
#   sha1.hexdigest      # => "813a169342f956720ff4333f5777e629b6cb4f9a"
module Digest

  autoload :MD5,  'digest/md5'
  autoload :SHA1, 'digest/sha1'
  autoload :SHA256, 'digest/sha256'
  autoload :SHA384, 'digest/sha384'
  autoload :SHA512, 'digest/sha512'

  # call-seq:
  #     Digest.hexencode(string) -> hexencoded_string
  #
  # Generates a hex-encoded version of a given _string_.
  def hexencode(value)
    unless value._isString
      value = Maglev::Type.coerce_to(value, String, :to_str)
    end
    out = ''
    value.each_byte { |b| out << sprintf("%02x", b) }
    out
  end
  module_function :hexencode

  # This module defines the API each digest implementation must support.
  # Provides default implementation for many of the methods.
  module Instance
    # call-seq:
    #     digest_obj.update(string) -> digest_obj
    #     digest_obj << string -> digest_obj
    #
    # Updates the digest using a given +string+ and returns self.
    #
    # The update() method and the left-shift operator are overridden by
    # each implementation subclass. (One should be an alias for the
    # other)
    def update(arg)
      raise NotImplementedError, "#{self.class.name} does not implement update()"
    end

    # An alais for #update.
    def <<(arg)
      raise NotImplementedError, "#{self.class.name} does not implement <<()"
    end

    # call-seq:
    #     digest_obj.instance_eval { finish } -> digest_obj
    #
    # Finishes the digest and returns the resulting hash value.
    #
    # This method is overridden by each implementation subclass and often
    # made private, because some of those subclasses may leave internal
    # data uninitialized.  Do not call this method from outside.  Use
    # #digest!() instead, which ensures that internal data be reset for
    # security reasons.
    def finish
      raise NotImplementedError, "#{self.class.name} does not implement finish()"
    end

    # call-seq:
    #     digest_obj.reset -> digest_obj
    #
    # Resets the digest to the initial state and returns self.
    #
    # This method is overridden by each implementation subclass.
    def reset
      raise NotImplementedError, "#{self.class.name} does not implement reset()"
    end

    # call-seq:
    #     digest_obj.digest_length -> integer
    #
    # Returns the length of the hash value of the digest.
    #
    # This method should be overridden by each implementation subclass.
    # If not, digest_obj.digest().length() is returned.
    def digest_length
      d = digest()
      unless d._isString
        d = Maglev::Type.coerce_to(d, String, :to_s)
      end
      d.length
    end

    # call-seq:
    #     digest_obj.block_length -> integer
    #
    # Returns the block length of the digest.
    #
    # This method is overridden by each implementation subclass.
    def block_length
      raise NotImplementedError, "#{self.class.name} does not implement block_length()"
    end

    # Instance methods that may be overridden

    # call-seq:
    #     digest_obj == another_digest_obj -> boolean
    #     digest_obj == string -> boolean
    #
    # If a string is given, checks whether it is equal to the hex-encoded
    # hash value of the digest object.  If another digest instance is
    # given, checks whether they have the same hash value.  Otherwise
    # returns false.
    def ==(other)
      str1 = hexdigest
      if other.kind_of?(Digest::Instance)
        str2 = other.hexdigest
      else
        str2 = other
      end
      unless str1._isString
        str1 = Maglev::Type.coerce_to(str1, String, :to_str)
      end
      unless str2._isString
        str2 = Maglev::Type.coerce_to(str2, String, :to_str)
      end
      str1.eql?(str2)
    end

    # call-seq:
    #     digest_obj.inspect -> string
    #
    # Creates a printable version of the digest object.
    def inspect
      "#<#{self.class}: #{self.hexdigest}>"
    end

    #--
    # Instance methods that are not normally overridden
    #++

    # call-seq:
    #     digest_obj.new -> another_digest_obj
    #
    # Returns a new, initialized copy of the digest object.  Equivalent
    # to digest_obj.clone().reset().
    def new
      clone = self.clone
      clone.reset
      clone
    end

    # call-seq:
    #     digest_obj.digest -> string
    #     digest_obj.digest(string) -> string
    #
    # If none is given, returns the resulting hash value of the digest,
    # keeping the digest's state.
    #
    # If a _string_ is given, returns the hash value for the given
    # _string_, resetting the digest to the initial state before and
    # after the process.
    def digest(string = MaglevUndefined)
      if string.equal?(MaglevUndefined)
        klone = self.clone
        value = klone.finish
        klone.reset
      else
        reset
        update(string)
        value = finish
        reset
      end
      value
    end

    # call-seq:
    #     digest_obj.digest! -> string
    #
    # Returns the resulting hash value and resets the digest to the
    # initial state.
    def digest!
      value = finish
      reset
      value
    end

    # call-seq:
    #     digest_obj.hexdigest -> string
    #     digest_obj.hexdigest(string) -> string
    #
    # If none is given, returns the resulting hash value of the digest in
    # a hex-encoded form, keeping the digest's state.
    #
    # If a _string_ is given, returns the hash value for the given
    # _string_ in a hex-encoded form, resetting the digest to the initial
    # state before and after the process.
    def hexdigest(string = MaglevUndefined)
      Digest.hexencode(digest(string))
    end

    # call-seq:
    #     digest_obj.hexdigest! -> string
    #
    # Returns the resulting hash value and resets the digest to the
    # initial state.
    def hexdigest!
      value = finish
      reset
      Digest.hexencode(value)
    end

    # call-seq:
    #     digest_obj.to_s -> string
    #
    # Returns digest_obj.hexdigest().
    def to_s
      hexdigest()
    end

    # call-seq:
    #     digest_obj.length -> integer
    #     digest_obj.size -> integer
    #
    # Returns digest_obj.digest_length().
    def length
      digest_length()
    end

    def size
      digest_length()
    end

  end

  # This module stands as a base class for digest implementation classes.
  class Class
    include Instance

    # call-seq:
    #     Digest::Class.digest(string, *parameters) -> hash_string
    # Returns the hash value of a given string. This is equivalent to
    # Digest::Class.new(*parameters).digest(string), where extra
    # parameters, if any, are passed through to the constructor and the
    # string is passed to digest().
    def self.digest(string, *params)
      new(*params).digest(string)
    end

    # call-seq:
    #     Digest::Class.hexdigest(string[, ...]) -> hash_string
    #
    # Returns the hex-encoded hash value of a given _string_.  This is
    # almost equivalent to
    # Digest.hexencode(Digest::Class.new(*parameters).digest(string)).
    def self.hexdigest(string)
      d = new(self.digest_name)
      sum = d.digest(string)
      ::Digest.hexencode(sum)
    end

    # creates a digest object and reads a given file, name.
    # E.g.,
    #   p Digest::SHA256.file("X11R6.8.2-src.tar.bz2").hexdigest
    #   => "f02e3c85572dc9ad7cb77c2a638e3be24cc1b5bea9fdbb0b0299c9668475c534"
    #
    def self.file(name)
      raise TypeError, "Nil passed" if name.nil?
      name_str = name.to_str
      raise Errno::EISDIR if File.directory?(name_str)
      f = File.open(name_str, 'r')
      contents = f.read(nil)
      self.new(contents)
    end
  end

  # This abstract class provides a common implementation for message
  # digests that will use the OpenSSL implementation.  Each derived class
  # should call new with the appropriate OpenSSL implementation, e.g.,:
  #
  # class ::Digest::MD5 < ::Digest::Base
  #   def self.md5(string)
  #     new(string)
  #   end
  #
  #   def initialize(str = nil)
  #     super(OpenSSL::Digest.const_get('MD5').new(str))
  #   end
  # end

  class Base < ::Digest::Class
    def initialize(impl)
      @impl = impl # OpenSSL::Digest.const_get(algo).new(str)
      self
    end

    def block_length
      @impl.block_length
    end

    def digest_length
      @impl.digest_length
    end

    alias :length :size

    def finish
      @impl.finish
    end

    def reset
      @impl.reset
    end

    def update(arg)
      @impl.update(arg)
    end
    alias :<< :update

    def digest(str = MaglevUndefined)
      @impl.digest(str)
    end

    private
    def to_str(bignum)
      l = digest_length
      bytes = Array.new(l)
      b = bignum
      l.times do |i|
        bytes[l - i - 1] =  b & 0xFF
        b = b >> 8
      end
      bytes.pack("C*")
    end

    # ['SHA256', 'SHA512'].each do |klass_name|
    #   klass = Class.new(::Digest::Base) do
    #     define_method(:initialize) do |str|
    #       super(OpenSSL::Digest.const_get(klass_name).new(str))
    #     end
    #
    #     singleton = (class << klass; self; end)
    #     singleton.class_eval{
    #       define_method(:"#{klass_name}") {|string| new(string) }
    #     }
    #   end
    #   const_set(klass_name, klass)
    # end
  end
end
require 'openssl'
