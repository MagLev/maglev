# class RubyTimeoutError  is defined in the maglev*.mcz
RUBY.global("TimeoutError", "RubyTimeoutError")

class TimeoutError
  class_primitive_nobridge 'timeout', 'timeout:do:'
end

def timeout(n, &b)
    TimeoutError.timeout(n, b)
end

module Timeout
    def timeout(n, &b)
        TimeoutError.timeout(n, b)
    end
    
    def self.timeout(n, &b)
        TimeoutError.timeout(n, b)
    end
end
