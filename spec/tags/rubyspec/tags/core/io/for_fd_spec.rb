fails:IO.for_fd creates an IO instance from a Fixnum argument
fails:IO.for_fd calls #to_int on an object to convert to a Fixnum
fails:IO.for_fd raises an Errno::EBADF if the file descriptor is not valid
fails:IO.for_fd raises an IOError if passed a closed stream
fails:IO.for_fd raises an Errno::EINVAL if the new mode is not compatible with the descriptor's current mode
