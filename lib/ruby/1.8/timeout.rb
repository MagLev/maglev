# TimeoutError defined in kernel/bootstrap/Exception.rb.

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
