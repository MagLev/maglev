class SystemCallError
  attr_reader :errno

#   class_primitive_nobridge '_new', 'basicNew'

#   def self.new(msg, errno)
#     instance = _new
#     instance.initialize(msg, errno)
#     instance
#   end

  def initialize(msg)
    super
    @errno = -1
  end

# TODO: ??
#   def ===(other)
#     puts "SystemCallError#==="
#     return false unless other.class == SystemCallError
#     return other.errno == errno
#   end
end

module Errno
  # Map an errno (small int) to the exception class for that errno
  ERRNO_TO_EXCEPTION = [ ]

  # Given a return value from a "system or library" call, raise the
  # appropriate exception if any.  If +err+ is not a small integer, or if
  # it is 0, then return +err+ (i.e., there was no problem, so just return
  # the status to caller). Otherwise, interpret +err+ as a Unix errno and
  # raise the correct exception.
  #
  # In the non-errno cases, handle returns err so that client code can be
  # written like:
  #     def Dir.getwd
  #       Errno.handle(_getwd, "Dir.getwd")
  #     end
  def self.handle(err, additional = nil)
    return err unless err._isSmallInteger  # Not an errno
    return err if err.equal?(0)                 # errno signifying no error

    errno_exc = ERRNO_TO_EXCEPTION[err]
    errno_exc ||= SystemCallError.new("System error (errno: #{err}): #{additional}", err)
    raise errno_exc
  end

  #    We may want to make Errno::EBADF::Errno map to a method call so that
  #    we can map platform specific errnos to the correct, shared,
  #    Errno::EBADF class.  We want one Smalltalk image to be usable on
  #    several platforms, so we need to make sure platform specific errno
  #    ints from the underlying platform map to the correct SystemCallError
  #    subclass.
  #
  #    Another way we could do this is to implement const_missing for the
  #    Errno::EBADF classes and have that call Errno.errno_for(sym).  See
  #    commented code below.
  #

  # following 4 exceptions may be signaled from smalltalk code.
  #   but they are subclasses of SocketError not SystemCallError...
  #   so don't use them here.
  # EBADF = _resolve_smalltalk_class(:SocketErrorEBADF)
  # ECONNRESET = _resolve_smalltalk_class(:SocketErrorECONNRESET)
  # ENOTCONN = _resolve_smalltalk_class(:SocketErrorENOTCONN)
  # EPIPE = _resolve_smalltalk_class(:SocketErrorEPIPE)

  # Create a class to represent +errno+.  A constant, +name+, will be added to
  # +Errno+ to store the class and the class will have the value of its
  # constant +Errno+ be +errno+.  E.g.,
  #   <tt>_createErrnoClass(9, 'EBADF')</tt>
  # will create :
  #
  #     <tt>Errno::EBADF         # => A class representing Errno number 9.</tt>
  #     <tt>Errno::EBADF::Errno  # => 9</tt>
  def self._createErrnoClassXX(errno, name)
    if const_defined?(name)
      klass = const_get(name)  # class already mapped to a Smalltalk class
    else
      klass = Class.new(SystemCallError)
      const_set(name, klass)
    end
    klass.const_set(:Errno, errno)
    klass
  end


  # TODO: There is the case where two error names, e.g., EBADF and
  # EBADFALSO, both map to the same errno (9).  To accommodate that, and
  # the semantics of rescue clauses, we make the second and subsequent
  # instances of an errno class subclasses of the first instance.

  def self._createErrnoClass(errno, name)
    klass = class_eval %Q{
      class #{name} < SystemCallError
        Errno = #{errno}
      end
      Errno::ERRNO_TO_EXCEPTION[#{errno}] = #{name}
    }
   klass
  end

  # Return an array of errno names, indexed by errno for this platform.
  def self.errno_names
    Exception._errnoTables[Exception._cpuOsKind - 1] # Adjust for smalltalk
  end

  def self.createAllErrnoClasses
    table = self.errno_names
    table.each_with_index do |name, st_errno|
      ruby_errno = st_errno + 1 # adjust from smalltalk indexing
      ERRNO_TO_EXCEPTION[ruby_errno] = self._createErrnoClass(ruby_errno, name) unless name.nil?
    end
  end

  createAllErrnoClasses
end
