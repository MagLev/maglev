# file: ffi.rb
#  FFI is loaded by the Maglev bootstrap , so this file only deals with
#  the system-specific things that have to be determined at runtime

module FFI
  module Library
    case Object.__platform_str
    when /darwin/i
      LIBC = "libc.dylib"
    # when /x86_64_Solaris/i # do we need this?
    else
      LIBC = "libc.so.6"
    end
  end
end
