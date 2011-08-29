module Errno
  # Map an errno (small int) to the exception class for that errno
  module MaglevErr
    ERRNO_TO_EXCEPTION = [ ]
    EXCEPTION_CLS_TO_ERRNO = nil
  end
end
Errno.__freeze_constants
