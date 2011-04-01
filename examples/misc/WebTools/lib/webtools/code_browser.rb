require 'webtools/ruby'

module WebTools
  # The CodeBrowser is a ViewModel for MagLev code browsing.  It keeps the
  # entire state of the UI and returns structured data for use by a UI.
  # For the first pass, we do not cache anything.
  class CodeBrowser
    def self.class_and_module_list
      puts "#{self}.class_and_module_list()"
      { 'modules' => WebTools::Ruby.class_and_module_names }
    end

    def select_module(module_name)
      puts "#{self}.select_module(#{module_name.inspect})"
      # At some point, if we need to cache individual classes/modules, or
      # if the state becomes complex, we could introduce a ModuleInfo class
      # to store the currently selected class/module.  Until then, this
      # suffices.
      mod = Ruby.find_in_namespace(module_name)
      @const_value        = nil
      @constants          = mod.constants.sort
      @instance_methods   = mod.instance_methods(false).sort
      @is_instance_method = nil
      @module_methods     = Ruby.module_fns_for(mod)
      @selected_constant  = nil
      @selected_method    = nil
      @selected_module    = module_name
      state
    end

    def select_constant(module_name, const_name)
      raise 'Module mismatch' unless @selected_module.nil? or @selected_module == module_name
      @selected_constant = const_name
      @const_value = Ruby.const_info_for(module_name, const_name)
      { :selected_constant => @selected_constant,
        :const_value       => @const_value }
    end

    def select_method(module_name, method_name, is_instance_method)
      raise 'Module mismatch' unless @selected_module.nil? or @selected_module == module_name
      mod = Ruby.find_in_namespace(module_name)
      src, file, line = mod.method_source(method_name, is_instance_method)
      @selected_method = method_name
      @is_instance_method = is_instance_method
      { :selected_method    => @selected_method,
        :is_instance_method => @is_instance_method,
        :method_source      => src,
        :method_source_file => file,
        :method_line_number => line }
    end

    def state
      {
        :const_value        => @const_value,
        :constants          => @constants         || [],
        :instance_methods   => @instance_methods  || [],
        :is_instance_method => @is_instance_method,
        :module_methods     => @module_methods    || [],
        :selected_constant  => @selected_constant,
        :selected_method    => @selected_method,
        :selected_module    => @selected_module,
      }
    end
  end
end
