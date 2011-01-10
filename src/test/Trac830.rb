# Based on a bug found using rake under MagLev.
#
# $ cd $MAGLEV_HOME/examples/sinatra/simple_blog/
# $ rake test
# (in /Users/monty/MagLev/MagLev-24990.Darwin-i386/examples/sinatra/simple_blog)
#  maglev-ruby -Ilib lib/commit_code.rb  
# rake aborted!
# NoMethodError: undefined method `call' for aMetaModule
#   end
# end

module M
  class << self
    def define_module_function(name, &block)
      ax = define_method(name, &block)
      bx = module_function(name)
    end
  end

  mx = Kernel.method(:system)
  define_module_function(:kernel_system, &mx )
end

M::kernel_system("echo foo")  # Raises: NoMethodError: undefined method `call' for aMetaModule
true

