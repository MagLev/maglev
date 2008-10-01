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

class LoadError
  def self.name
    # override Smalltalk name
    'LoadError'
  end
end

class SystemExit
  def self.name
    # override Smalltalk name
    'SystemExit'
  end
end

class StandardError
  def self.name
    # override Smalltalk name
    'StandardError'
  end
end

class EBADF
  def self.name
    # override Smalltalk name
    'EBADF'
  end
end

class ENOTCONN
  def self.name
    # override Smalltalk name
    'ENOTCONN'
  end
end

class EPIPE
  def self.name
    # override Smalltalk name
    'EPIPE'
  end
end

class ECONNRESET
  def self.name
    # override Smalltalk name
    'ECONNRESET'
  end
end

