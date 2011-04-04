require 'webtools/ruby'
require 'json'

module WebTools
  # The CodeBrowser is a ViewModel for MagLev code browsing.  It keeps the
  # entire state of the UI and returns structured data for use by a UI.
  # For the first pass, we do not cache anything.
  class CodeBrowser
    def self.class_and_module_list
      { 'modules' => WebTools::Ruby.class_and_module_names }
    end

    def select_module(module_name)
      mod = Ruby.find_in_namespace(module_name)
      {
        :ancestors          => mod.ancestors.reverse,
        :constants          => mod.constants.sort,
        :instance_methods   => mod.instance_methods(false).sort,
        :module_methods     => Ruby.module_fns_for(mod),
        :selected_module    => module_name,
      }
    end

    def select_constant(module_name, const_name)
      parent = Ruby.find_in_namespace module_name
      {
        :const_value       => ObjectInfo.for(parent.const_get(const_name)),
        :selected_constant => const_name,
        :selected_module   => module_name,
      }
    end

    def select_method(module_name, method_name, is_instance_method)
      mod = Ruby.find_in_namespace(module_name)
      src, file, line = mod.method_source(method_name, is_instance_method)
      {
        :is_instance_method => is_instance_method,
        :method_line_number => line,
        :method_source      => src,
        :method_source_file => file,
        :module_name        => module_name,
        :selected_method    => method_name,
      }
    end

    def object_info(object_id)
      ObjectInfo.for(object_id)
    end
  end

  class ObjectInfo
    # Return a new ObjectInfo instance for the object with the given object
    # id.
    def self.for_id(object_id)
      new(ObjectSpace._id2ref(oop))
    end

    # Return a new ObjectInfo instance for the given object.
    def self.for(object)
      new(object)
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
