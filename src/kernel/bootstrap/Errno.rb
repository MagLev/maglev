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
end

