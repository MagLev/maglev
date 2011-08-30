module Errno
  module MaglevErr
    # ERRNO_TO_EXCEPTION = [ ]  # initialized in Errno1.rb
    EXCEPTION_CLS_TO_ERRNO = IdentityHash.new  # actual value

    def self.__new_for_errno(errno)
      cls = ERRNO_TO_EXCEPTION[errno]
      if cls._equal?(nil)
	return nil
      end
      exc = cls.allocate
      exc.__st_initialize
      exc.errno=(errno)
      m = cls.__default_ruby_message
      if m._not_equal?(nil)
	exc.__message=(m)
      end
      exc
    end

    def self.__errno_for_class(cls)
      EXCEPTION_CLS_TO_ERRNO[cls]
    end
  end

  def self.__new_for_errno(errno)
    MaglevErr::__new_for_errno(errno)
  end

  # Map an errno (small int) to the exception class for that errno

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
  def self.handle(err, additional='')
    return err unless err._isSmallInteger  # Not an errno
    return err if err._equal?(0)                 # errno signifying no error
    self.raise_errno(err, additional)
  end

  def self.handle
    errnum = Dir.__get_clear_errno
    self.handle(errnum, '')
  end

  def self.raise_errno(errno, additional='')
    exc = MaglevErr.__new_for_errno(errno)
    if exc._equal?(nil)
      exc = SystemCallError.new("System error (errno: #{errno}):" , errno)
    end
    if additional._isString && additional.length > 0
      exc.__message_append(', ')
      exc.__message_append(additional)
    end
    exc.__signal
  end
  private

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
  # EBADF = __resolve_smalltalk_global(:SocketErrorEBADF)
  # ECONNRESET = __resolve_smalltalk_global(:SocketErrorECONNRESET)
  # ENOTCONN = __resolve_smalltalk_global(:SocketErrorENOTCONN)
  # EPIPE = __resolve_smalltalk_global(:SocketErrorEPIPE)

  # Create a class to represent +errno+.  A constant, +name+, will be added to
  # +Errno+ to store the class and the class will have the value of its
  # constant +Errno+ be +errno+.  E.g.,
  #   <tt>_createErrnoClass(9, 'EBADF')</tt>
  # will create :
  #
  #     <tt>Errno::EBADF         # => A class representing Errno number 9.</tt>
  #     <tt>Errno::EBADF::Errno  # => 9</tt>

  # TODO: There is the case where two error names, e.g., EBADF and
  # EBADFALSO, both map to the same errno (9).  To accommodate that, and
  # the semantics of rescue clauses, we make the second and subsequent
  # instances of an errno class subclasses of the first instance.

  def self.__create_errno_class(errno, name)
    sklass = MaglevErr::ERRNO_TO_EXCEPTION[errno] || SystemCallError
    klass = Class.new(sklass)             # create class
    const_set(name, klass)                # Assign class name
    klass.const_set(:Errno, errno)        # Set class's Errno constant
    MaglevErr::ERRNO_TO_EXCEPTION[errno] = klass
    MaglevErr::EXCEPTION_CLS_TO_ERRNO[klass] = errno
    klass
  end

  # Return an array of errno names, indexed by errno for this platform.
  def self.errno_names
    Exception.__errno_tables[Exception.__cpu_os_kind - 1] # Adjust for smalltalk
  end

  def self.__create_all_errno_classes
    table = self.errno_names
    table.each_with_index do |name, st_errno|
      ruby_errno = st_errno + 1 # adjust from smalltalk indexing
      self.__create_errno_class(ruby_errno, name) unless name._equal?(nil)
    end
    # Special cases: Some systems spel "EACCESS" as "EACCES", but user code
    # uses the longer name.  Note: Kernel#defined? is not defined during bootstrap,
    # so munge through the errno tables
    if ! table.include?('EACCESS') && table.include?('EACCES')
      const_set('EACCESS', Errno::EACCES)
      # TODO: this is broken during bootstrap
      #      __create_errno_class(Errno::EACCES::Errno, 'EACCESS')
    end
  end

  __create_all_errno_classes

  # override definition of EAGAIN so Smalltalk socket code
  # can signal using   SocketErrorEAGAIN .
  EAGAIN = __resolve_smalltalk_global(:SocketErrorEAGAIN)
  EWOULDBLOCK = __resolve_smalltalk_global(:SocketErrorEAGAIN)  # Trac 818
end

# Create Errno specific error messages here
class Errno::ENOENT
  def self.__default_ruby_message
    "No such file or directory - "
  end
end
