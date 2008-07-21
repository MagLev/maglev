RUBY.global("TimeoutError", "RubyTimeoutError")
TimeoutError.class.primitive 'timeout', 'timeout:do:'

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