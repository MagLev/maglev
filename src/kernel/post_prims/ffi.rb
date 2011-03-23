module FFI
  
  DEBUG = 0   # 0 = no printing
              # 1 = print where functions found
              # 2 = print full library search info 

  module Platform
    transient_const_set( :OS ) { Exception.__cpu_os_str }
    transient_const_set( :ARCH ) { `uname -m`.chomp } # same as rbconfig.rb

    transient_const_set( :LIBC ) {
      if RUBY_PLATFORM == 'x86_64-linux'
	 '/lib/libc.so.6'  # as of Ubuntu 2.6.32
      else
	'libc'
      end
    }

  end
end

