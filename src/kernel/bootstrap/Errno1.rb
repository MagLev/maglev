module Errno
  # Map an errno (small int) to the exception class for that errno
  ERRNO_TO_EXCEPTION = [ ]
  EXCEPTION_CLS_TO_ERRNO = IdentityHash.new
end
