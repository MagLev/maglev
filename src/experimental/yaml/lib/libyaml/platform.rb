module FFI
  class NativeError < RuntimeError
  end
end

# require 'rbconfig'
module FFI
  module Platform
    OS = "darwin"
    ARCH = "x86_64"
    NAME = "#{ARCH}-#{OS}"
    IS_LINUX = false
    IS_MAC = true
    IS_FREEBSD = false
    IS_OPENBSD = false
    IS_WINDOWS = false
    IS_BSD = true
    LIBC = 'c'
    LIBPREFIX = "lib"
    LIBSUFFIX = "dylib" # OS  dependent suffix appended at runtime by the
                   # library load primitives if '.' not in lib name,
                   #    or if '.' is last character of lib name.
    LONG_SIZE = 64 # in bits
    ADDRESS_SIZE = 64
    def self.bsd?
      IS_BSD
    end

    def self.windows?
      IS_WINDOWS
    end

    def self.mac?
      IS_MAC
    end

    def self.unix?
      !IS_WINDOWS
    end
  end
end

