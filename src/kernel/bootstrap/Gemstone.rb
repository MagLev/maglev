
# Class Gemstone is identically Smalltalk System
class Gemstone

  # Transaction support
  class_primitive '_commitTransaction', 'commitTransaction'
  class_primitive '_abortTransaction', 'abortTransaction'
  class_primitive '_beginTransaction', 'beginTransaction'

  def self.commitTransaction
    # STDERR.write( "- Ruby  commitTransaction\n") 
    _commitTransaction
  end
  def self.abortTransaction
    # STDERR.write( "- Ruby  abortTransaction\n") 
    _abortTransaction
  end
  def self.beginTransaction
    # STDERR.write( "- Ruby  beginTransaction\n") 
    _beginTransaction
  end

  class_primitive 'session_temp' , '_sessionTempsAt:'  # returns nil if not found
  class_primitive 'session_temp_put', '_sessionTempsAt:put:' # returns value stored

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
