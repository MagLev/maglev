# This file starts off the same as Trac494.rb, but the second call to
# define_module_function is new and has a bug.
#
# Rake is blocked on this.
module G
  class << self
    def define_module_function(name, &block)
      define_method(name, &block)
      module_function(name)
    end
  end
  define_module_function(:system, &Kernel.method(:system))
  define_module_function(:'`', &Kernel.method(:'`'))
end

