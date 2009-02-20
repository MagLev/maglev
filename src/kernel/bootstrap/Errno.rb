module Errno
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

    # TODO: Errno: need to map errno to correct Exception
    raise SystemCallError, "System error (errno: #{err}): #{additional}"
  end

  # TODO: Errno.rb: Map system dependent errno ints to common Errno class

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
  # Create a class to represent +errno+.  A constant, +name+, will be added to
  # +Errno+ to store the class and the class will have the value of its
  # constant +Errno+ be +errno+.  E.g.,
  #   <tt>_createErrnoClass(9, 'EBADF')</tt>
  # will create :
  #
  #     <tt>Errno::EBADF         # => A class representing Errno number 9.</tt>
  #     <tt>Errno::EBADF::Errno  # => 9</tt>

  # following 4 exceptions may be signaled from smalltalk code.
  #   but they are subclasses of SocketError not SystemCallError... 
  #   so don't use them here.  
  # EBADF = _resolve_smalltalk_class(:SocketErrorEBADF)
  # ECONNRESET = _resolve_smalltalk_class(:SocketErrorECONNRESET)
  # ENOTCONN = _resolve_smalltalk_class(:SocketErrorENOTCONN)
  # EPIPE = _resolve_smalltalk_class(:SocketErrorEPIPE)

  def self._createErrnoClass(errno, name)
    if const_defined?(name)
      klass = const_get(name)  # class already mapped to a Smalltalk class
      nil.pause
    else
      klass = Class.new(SystemCallError)
      const_set(name, klass)
    end
    klass.const_set(:Errno, errno)  

    # If we want to add a const_missing hook, then comment out the above
    # line and do something like this instead:
#     klass.instance_eval do
#       def const_missing(sym)
#         if sym.to_str == 'Errno'
#           my_name = self.name.split('::')[-1]
#           Errno.errno_names.index[my_name] + 1
#         else
#           raise NameError, "uninitialized constant #{self}::#{sym}"
#         end
#       end
#     end
  end

  # Return an array of errno names, indexed by errno for this platform.
  def self.errno_names
    Exception._errnoTables[Exception._cpuOsKind - 1] # Adjust for smalltalk
  end

  def self.createAllErrnoClasses
    table = self.errno_names
    table.each_with_index do |name, errno|
      # 'errno + 1': adjust from smalltalk indexing
      self._createErrnoClass(errno + 1, name)  unless name.nil?
    end
  end
end

Errno.createAllErrnoClasses

