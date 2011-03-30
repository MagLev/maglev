require 'maglev/method_source' if defined? Maglev
module WebTools
  # Provide information about the Ruby environment
  module Ruby
    # Traverse the Ruby namespace hierarchy and execute block for all classes
    # and modules.  Returns an IdentitySet of all classes and modules found.
    # Skips autoloads (i.e., does not trigger them and does not yield them to
    # the block).
    #
    # @param [Module] klass The Class or Module object to start traversal.
    #         Default is Object.
    #
    # @param [IdentitySet] rg The recursion guard used to prevent infinite
    #         loops; also used as return value.
    #
    # @return [IdentitySet] An IdentitySet of all the Classes and Modules
    #         registered in the Ruby namespace
    #
    def each_module(klass=Object, rg=IdentitySet.new, &block)
      raise 'Must be class or module' unless Module === klass
      unless rg.include?(klass)
        rg.add klass
        yield klass
        klass.constants.each do |c|
          unless klass.autoload?(c)
            obj = klass.const_get c
            each_module(obj, rg, &block) if Module === obj
          end
        end
      end
      rg
    end
    module_function :each_module

    # Returns a sorted list of class and module names in the Ruby Namespace
    #
    # @return [Array] sorted list of class and module names found in the Ruby
    #         namespace hierarchy.
    def class_and_module_names
      names = []
      each_module { |klass|  names << klass.name }
      names.sort
    end
    module_function :class_and_module_names

    # A simple message from Ruby Land.
    def say_something
      "Hello from MagLev, My name is '#{self.name}'"
    end
    module_function :say_something

    # Return an object named in the Ruby namespace.
    #
    # @param [String] name The name of the object. E.g., "Object",
    #         "Errno::EACCES", "Foo::Bar::Baz".
    #
    # @return [Object] the named object.
    #
    # @raise [NameError] if the name can't be found
    def find_in_namespace(name)
      name.split('::').inject(Object) do |parent, name|
        obj = parent.const_get name
      end
    end
    module_function :find_in_namespace

    MODULE_MOD_FNS = Module.instance_methods(false)

    def module_info_for(name)
      mod = find_in_namespace name
      { :constants        => mod.constants.sort,
        :class_methods    => module_fns_for(mod),
        :instance_methods => mod.instance_methods(false).sort }
    end
    module_function :module_info_for

    def module_fns_for(mod)
      res = []
      begin
        sclass = mod.__singleton_class
        res = sclass.instance_methods(false) - MODULE_MOD_FNS
      rescue => e
        # nothing
      end
      res
    end
    module_function :module_fns_for

    # Get the source code for the named class and method
    #
    # The success of this method depends on being called from a Ruby stack,
    # not a Smalltalk stack.
    #
    # @param [String] The name of the class
    # @param [String] The method name
    # @param [Boolean] if true, then look for a class method, otherwise look
    #         for an instance method.
    def source_for(class_name, method_name, instance_method=true)
puts "======= source_for(#{class_name}, #{method_name}, #{instance_method})"
      klass = find_in_namespace(class_name)
      gsnmeth = klass.gs_method_for(method_name, instance_method)
      src = gsnmeth.__source_string
      file,line = gsnmeth.__source_location
      file.nil? ? "#{src}\n# No file information available." : "#{src}\n##{file}:#{line}"
    end
    module_function :source_for

    # Return a display string suitable for rendering a constant.
    # Currently, it just returns the results of #inspect on the object.
    #
    # TODO
    #   1. If the const value is a proc, then return "Proc: " + the source code.
    #
    # @param [String,Symbol] class_name The name of the class to query.
    # @param [String,Symbol] const_name The name of the constant.
    # @return [String] A description of the constant.
    #
    def const_info_for(class_name, const_name)
      klass = find_in_namespace class_name
      const = klass.const_get const_name
      const.inspect
    end
    module_function :const_info_for

    # def logit(msg)
    #   File.open("/tmp/log", "a+") do |f|
    #     f.puts msg
    #     f.flush
    #   end
    # end
    # module_function :logit
  end
end
