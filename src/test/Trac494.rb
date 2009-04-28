module G
  class << self
    def define_module_function(name, &block)
      define_method(name, &block)
      module_function(name)
    end
  end
  define_module_function :system, &Kernel.method(:system)
end


