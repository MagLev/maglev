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
    return err if err == 0                 # errno signifying no error

    # TODO: Errno: need to map errno to correct Exception
    raise SystemCallError, "System error (errno: #{err}): #{additional}"
  end

#   # TODO: Errno.rb: errno class support Waiting on:
#   #   1: Hierarchical namespace support from Allen
#   #   2: Class.new support
#   #   3: Support for assigning to constants will set a class name
#   #   4: We may want to make Errno::EBADF::Errno map to a method call so
#   #      that we can map platform specific errnos to the correct, shared,
#   #      Errno::EBADF class.  We want one Smalltalk image to be usable on
#   #      several platforms, so we need to make sure platform specific errno
#   #      ints from the underlying platform map to the correct SystemCallError
#   #      subclass.
#   # Create a class to represent +errno+.  A constant, +name+, will be added to
#   # +Errno+ to store the class and the class will have the value of its
#   # constant +Errno+ be +errno+.  E.g.,
#   #   <tt>_createErrnoClass(9, 'EBADF')</tt>
#   # will create :
#   #
#   #     <tt>Errno::EBADF         # => A class representing Errno number 9.</tt>
#   #     <tt>Errno::EBADF::Errno  # => 9</tt>
#   def self._createErrnoClass(errno, name)
#     klass = Class.new(SystemCallError)
#     klass.const_set(:Errno, errno)
#     const_set(name, klass)
#   end

#   # Return an array of errno names, indexed by errno for this platform.
#   def self.errno_names
#     Exception._errnoTables[Exception._cpuOsKind - 1] # Adjust for smalltalk
#   end

#   def self.createAllErrnoClasses
#     table = Errno.errno_names
#     table.each_with_index { |name, errno| Errno._createErrnoClass(errno, name)}
#   end
end

# Waiting for support
# Errno.createAllErrnoClasses

