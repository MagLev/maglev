# file delta/Object3.rb

class Object 
  def self.__platform_str
    # used by rbconfig for Config['host_os']
    %w( not_used sparc_solaris linux-gnu PowerPC_AIX
                     darwin9.0 x86_64_Solaris Itanium_HP-UX
                    )[Exception.__cpu_os_kind - 1]
  end

  def self.__set_platform
     self.transient_const_set( :RUBY_PLATFORM ) { Exception.__cpu_os_str }
     self.transient_const_set( :PLATFORM ) { Exception.__cpu_os_str }
     self.transient_const_set(  :RandomInstance ) { RandomNp.new }
  end
end
Object.__set_platform

