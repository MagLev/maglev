class Exception
    primitive 'signal', 'signal:'
    self.class.primitive 'signal', 'signal:'
    primitive 'message', 'description'
    primitive_nobridge '_backtrace', 'backtrace'
   
    def self.name
      # override Smalltalk name
      'Exception'
    end

    def backtrace
      result = []
      _backtrace.each do |ary|
        where, line, source = ary
        if /(.*) \(envId 1\)/ =~ where
          meth = $1
          if source
            lines = source.split("\n").grep(/# method/)
            unless lines.empty?
              if /line (\d+) .* file (.*)/=~ lines[-1]
                baseline = $1.to_i
                file = $2
                result << "#{file[0..-2]}:#{baseline+line}: in '#{meth}'"
              end
            end
          end
        end
      end
      result[1..-1]
    end
end


class SystemExit
  def self.name
    'SystemExit'  # override Smalltalk name
  end
end

class SystemStackExit
  def self.name
    'SystemStackExit' # override Smalltalk name
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

