fails:IO.open creates an IO instance from a Fixnum argument
fails:IO.open calls #to_int on an object to convert to a Fixnum
fails:IO.open raises an Errno::EBADF if the file descriptor is not valid
fails:IO.open raises an IOError if passed a closed stream
fails:IO.open raises an Errno::EINVAL if the new mode is not compatible with the descriptor's current mode
fails:IO.open calls #close after yielding to the block
fails:IO.open propagates an exception raised by #close that is not a StandardError
fails:IO.open does not propagate a StandardError raised by #close
fails:IO.open does not set last error when a StandardError raised by #close
