fails:IO.new creates an IO instance from a Fixnum argument
fails:IO.new calls #to_int on an object to convert to a Fixnum
fails:IO.new raises an Errno::EBADF if the file descriptor is not valid
fails:IO.new raises an IOError if passed a closed stream
fails:IO.new raises an Errno::EINVAL if the new mode is not compatible with the descriptor's current mode
