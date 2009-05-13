# Maps to Smalltalk class UserException.  See Globals.rb
class Exception
    class_primitive 'allocate', 'rubyBasicNew'
    class_primitive '_signal', 'signal:'
    class_primitive_nobridge '_signal', 'signal'

    class_primitive 'install_debug_block&', 'installDebugBlock:'

    # support for errno to Name translation
    class_primitive_nobridge  '_errnoTables', 'errnoTables'
    class_primitive_nobridge  '_errnoToName', 'errnoToName:'
    class_primitive_nobridge  '_cpuOsKind', 'cpuOsKind'

    primitive_nobridge '_handler_active' , '_handlerActive'

    primitive_nobridge '_reraise', '_rubyReraise'
    primitive          '_signal', 'signal:'
    primitive_nobridge '_signal', 'signal'
    primitive          'message', 'description'
    primitive_nobridge '_message=', 'messageText:'
    primitive_nobridge '_st_initialize', 'initialize'

    # Define this in ruby code so we get the full env1 creation hooks
    def self.exception(message=nil)
      self.new(message)
    end

    def self._validate(obj)
      # used in implementation of $! on LHS of assignment
      if (obj.kind_of?(Exception))
        return obj
      else
        raise ArgumentError, 'not an Exception'
      end
    end

    IncludeSmalltalkFrames = false;

    def initialize(message=nil)
      self._st_initialize  # initialize smalltak instvars
      if message.equal?(nil)
        message = self.class.name 
      end
      @messageText = message
    end

    def backtrace(limit = 1000)
      # excludes smalltalk frames from the result
      #  limit defines smalltalk stack depth at which to stop
      @stack || Thread._backtrace(IncludeSmalltalkFrames, limit)
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

    def set_backtrace(array)
      @stack = array
    end

    def to_s
      (m = message).equal?(nil) ? self.class.name : m
    end

    def to_str
      (m = message).equal?(nil) ? self.class.name : m
    end
end

# open each of the subclasses of Exception that map to
#   a Smalltalk class to initialize the ruby name space
#  order here matches order in  Exception(C)>>commentRubyMapping

class SystemExit
end

class SystemStackExit
end
class NoMemoryError
end
class ScriptError
end
class LoadError  # a subclass of ScriptError
end
class NotImplementedError  # a subclass of ScriptError
end
class SyntaxError  # a subclass of ScriptError
end
class StandardError
end
class ArgumentError
end
class IOError
end
class EOFError  # a subclass of IOError
end
class SocketError  # a subclass of IOError
end
class IndexError
end
class LocalJumpError
end
class RuntimeError
end
class RangeError
end
class FloatDomainError # a subclass of RangeError
end
class RegexpError
end
class SecurityError
end
class SystemCallError
end
class ThreadError
end
class TypeError
end
class ZeroDivisionError
end
class NameError
end
class NoMethodError  # a subclass of NameError
end
class SignalException
end
class Interrupt  # a subclass of SignalException
end

# This was in lib/ruby/1.8/timeout.rb, but class_primitive_nobridge is not
# allowed after boot, so the exception definition was moved here.
#
# class RubyTimeoutError  is defined in the maglev*.mcz
#   RubyTimeoutError deprecated , just use  lib/ruby/1.8/timeout.rb
# RUBY.global("TimeoutError", "RubyTimeoutError")
# class TimeoutError
#  class_primitive_nobridge 'timeout', 'timeout:do:'
# end
