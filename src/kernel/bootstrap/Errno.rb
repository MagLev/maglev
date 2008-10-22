module Errno
  # This method is in the Rubinius interface, so we're keeping the same
  # API.  Given an return value from a "system or library" call, raise the
  # appropriate exception.  If 
  def self.handle(err, additional = nil)
    return if err == 0  # No error
    # TODO: Errno: need to map errno to correct Exception
    raise SystemCallError, "System error (errno: #{err}): #{additional}"
  end
end

