require 'digest/md5'

#
# The Digest module wraps some common methods on various hashing
# algorithms.  The methods provide an algorithm agnostic API for dealing
# with the digests.  Each implementation of a Digest algorithm (MD5, SHA1,
# etc.) implements the following methods:
#
#    reset()      # Reset state
#    finish()     # finalize state, calculate and return the hash
#    update(str)  # Add more data to the state
#    <<(str)      # alias :<< :update
#    block_length #
#
module Digest

  # call-seq:
  #     Digest.hexencode(string) -> hexencoded_string
  #
  # Generates a hex-encoded version of a given _string_.
  def hexencode(value)
    out = ''
    value.each_byte { |b| out << sprintf("%02x", b) }
    out
  end
  module_function :hexencode

#   def const_missing(c)
#     # Recognize :SHA256, :SHA384, :SHA512 and load up the right libs
#     # then return Digest.const_get(c)
#     raise NotImplementedError
#   end

  # This module provides instance methods for a digest implementation
  # object to calculate message digest values.
  module Instance
    # Instance methods that *should* be overridden

    # call-seq:
    #     digest_obj.update(string) -> digest_obj
    #     digest_obj << string -> digest_obj
    #
    # Updates the digest using a given _string_ and returns self.
    #
    # The update() method and the left-shift operator are overridden by
    # each implementation subclass. (One should be an alias for the
    # other)
    def update(arg)
      raise NotImplementedError, "#{self} does not implement update()"
    end
    def <<(arg)
      raise NotImplementedError, "#{self} does not implement <<()"
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
      raise NotImplementedError, "#{self} does not implement finish()"
    end

    # call-seq:
    #     digest_obj.reset -> digest_obj
    #
    # Resets the digest to the initial state and returns self.
    #
    # This method is overridden by each implementation subclass.
    def reset
      raise NotImplementedError, "#{self} does not implement reset()"
    end

    # call-seq:
    #     digest_obj.digest_length -> integer
    #
    # Returns the length of the hash value of the digest.
    #
    # This method should be overridden by each implementation subclass.
    # If not, digest_obj.digest().length() is returned.
    def digest_length
      d = Type.coerce_to(digest(), String, :to_s)
      d.length
    end

    # call-seq:
    #     digest_obj.block_length -> integer
    #
    # Returns the block length of the digest.
    #
    # This method is overridden by each implementation subclass.
    def block_length
      raise NotImplementedError, "#{self} does not implement block_length()"
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
      str1 = Type.coerce_to(digest, String, :to_str)
      if other.kind_of?(Digest::Instance)
        str2 = Type.coerce_to(other.digest, String, :to_str)
      else
        str2 = Type.coerce_to(other, String, :to_str)
      end
      str1.eql?(str2)
    end

    # call-seq:
    #     digest_obj.inspect -> string
    #
    # Creates a printable version of the digest object.
    def inspect
      #  #<Digest::ClassName: xxxxx...xxxx>
      "#<#{self.class}: #{self.hexdigest}>"
    end

    # Instance methods that are not normally overridden

    # call-seq:
    #     digest_obj.new -> another_digest_obj
    #
    # Returns a new, initialized copy of the digest object.  Equivalent
    # to digest_obj.clone().reset().
    def new
      self.clone().reset()
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
    def digest(string = Undefined)
      if string.equal?(Undefined)
        # Currently, getting the digest is non-destructive, so just
        # calculate and return (don't bother cloning)
        klone = clone
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
    def hexdigest(string = Undefined)
      if string.equal?(Undefined)
        # Currently, getting the digest is non-destructive, so just
        # calculate and return (don't bother cloning)
        klone = clone
        value = klone.finish
        klone.reset
      else
        reset
        update(string)
        value = finish
        reset
      end
      Digest.hexencode(value)
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
    def self.hexdigest(string, *rest)
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
    # Right now, we only support MD5, so all implementation is in md5.rb
  end
end
