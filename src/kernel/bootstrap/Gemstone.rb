
# Class Gemstone is identically Smalltalk System
class Gemstone

  # Transaction support
  class_primitive 'commitTransaction'
  class_primitive 'abortTransaction'
  class_primitive 'beginTransaction'

  # _processInfo includes getuid getgid, getpid, kill  and related functions
  class_primitive_nobridge '_processInfo', '_processInfo:with:with:'
  class_primitive_nobridge '_host_times', '_hostTimes'

  # Return the real uid for the server process
  def self.getuid
    _processInfo(0, nil, nil)
  end

  # Return the effective uid for the server process
  def self.geteuid
    _processInfo(1, nil, nil)
  end

  # Return the real group id for the server process
  def self.getgid
    _processInfo(2, nil, nil)
  end

  # Return the effective group id for the server process
  def self.getegid
    _processInfo(3, nil, nil)
  end

  # Return the process id for the server process
  def self.getpid
    _processInfo(8, nil, nil)
  end

  # Return the process group id for the server process
  def self.getpgrp
    _processInfo(9, nil, nil)
  end

  # Return the parent process id for the server process
  def self.getppid
    _processInfo(10, nil, nil)
  end

end
