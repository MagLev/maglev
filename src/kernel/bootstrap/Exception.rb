# Maps to Smalltalk class UserException.  See Globals.rb
class Exception
    class_primitive 'exception', 'new'
    class_primitive 'signal', 'signal:'

    # support for errno to Name translation
    class_primitive_nobridge  '_errnoTables', 'errnoTables'
    class_primitive_nobridge  '_errnoToName', 'errnoToName:'
    class_primitive_nobridge  '_cpuOsKind', 'cpuOsKind'

    def self.name
      # override Smalltalk name
      'Exception'
    end

    primitive 'signal', 'signal:'
    primitive 'message', 'description'

    def backtrace(limit = 1000)
      # excludes smalltalk frames from the result
      #  limit defines smalltalk stack depth at which to stop
      Thread._backtrace(false, limit)
    end

    def backtrace_st(limit = 1000)
      # include smalltalk frames in the result
      #  limit defines smalltalk stack depth at which to stop
      Thread._backtrace(true, limit)
    end

    primitive_nobridge '_message', 'messageText:'
    def exception(message = Undefined)
      return self if message.equal?(Undefined)
      e = dup
      e._message(message)
      e
    end

    # MNI: Exception#set_backtrace

    def to_s
      (m = message).nil? ? self.class.name : m
    end

    def to_str
      (m = message).nil? ? self.class.name : m
    end
end


class SystemExit
  def self.name
    'SystemExit'  # override Smalltalk name
  end
  # MNI: SystemExit#status
  # MNI: SystemExit#success?
end

class SystemStackError
  def self.name
    'SystemStackError' # override Smalltalk name
  end
end

class NoMemoryError
  def self.name
    'NoMemoryError' # override Smalltalk name
  end
end
class ScriptError
  def self.name
    'ScriptError' # override Smalltalk name
  end
end
class LoadError
  def self.name
    'LoadError' # override Smalltalk name
  end
end
class NotImplementedError
  def self.name
    'NotImplementedError' # override Smalltalk name
  end
end
class SyntaxError
  def self.name
    'SyntaxError' # override Smalltalk name
  end
end

class StandardError
  def self.name
    'StandardError' # override Smalltalk name
  end
end

class EBADF
  def self.name
    'EBADF' # override Smalltalk name
  end
end

class ENOTCONN
  def self.name
    'ENOTCONN' # override Smalltalk name
  end
end

class EPIPE
  def self.name
    'EPIPE' # override Smalltalk name
  end
end

class ECONNRESET
  def self.name
    'ECONNRESET' # override Smalltalk name
  end
end

class IndexError
  def self.name
    'IndexError' # override Smalltalk name
  end
end
class LocalJumpError
  def self.name
    'LocalJumpError' # override Smalltalk name
  end
end
class FloatDomainError
  def self.name
    'FloatDomainError' # override Smalltalk name
  end
end
class ZeroDivisionError
  def self.name
    'ZeroDivisionError' # override Smalltalk name
  end
end
class NoMethodError
  def self.name
    'NoMethodError' # override Smalltalk name
  end
end
class SignalException
  def self.name
    'SignalException' # override Smalltalk name
  end
end

