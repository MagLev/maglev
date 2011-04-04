require 'webtools/ruby'
require 'json'

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
      return if module_name == @select_module
      # At some point, if we need to cache individual classes/modules, or
      # if the state becomes complex, we could introduce a ModuleInfo class
      # to store the currently selected class/module.  Until then, this
      # suffices.
      mod = Ruby.find_in_namespace(module_name)
      @const_value        = nil
      @constants          = mod.constants.sort
      @instance_methods   = mod.instance_methods(false).sort
      @ancestors          = mod.ancestors.reverse
      @is_instance_method = nil
      @module_methods     = Ruby.module_fns_for(mod)
      @selected_constant  = nil
      @selected_method    = nil
      @selected_module    = module_name
      state
    end

    def select_constant(module_name, const_name)
      select_module(module_name)
      @selected_constant = const_name
      @const_value = ObjectInfo.for_const(module_name, const_name)
      { :selected_constant => @selected_constant,
        :const_value       => @const_value }
    end

    def select_method(module_name, method_name, is_instance_method)
      select_module(module_name)
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
        :ancestors          => @ancestors || [],
        :instance_methods   => @instance_methods  || [],
        :is_instance_method => @is_instance_method,
        :module_methods     => @module_methods    || [],
        :selected_constant  => @selected_constant,
        :selected_method    => @selected_method,
        :selected_module    => @selected_module,
      }
    end
  end

  class ObjectInfo
    def self.for_const(module_path, const_name)
      parent = Ruby.find_in_namespace module_path
      const = parent.const_get const_name
      new(const)
    end

    def initialize(obj)
      @obj = obj
    end

    def to_json(*args)
      info = { }
      info[:object_id] = @obj.object_id
      info[:class]     = @obj.class.name
      info[:inspect]   = @obj.inspect
      inst_vars = []
      @obj.instance_variables.each do |iv|
        puts "====== Adding inst var #{iv}"
        val = @obj.instance_variable_get(iv)
        inst_vars << [iv, val.inspect, val.object_id]
      end
      info[:instance_variables] = inst_vars

      enum_info = info[:enumerated] = []
      case @obj
      when Enumerable, Array
        @obj.each_with_index do |o,i|
          if i > 20
            enum_info << ['', '...']
            break
          else
            enum_info << [i, o.inspect]
          end
        end

      when Hash
        @obj.each do |k,v|
          enum_info << [k.to_s, v.inspect]
        end
      end

      info.to_json
    end
  end
end
