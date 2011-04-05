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
      methods = mod.instance_methods(false) +
        mod.protected_instance_methods(false) +
        mod.private_instance_methods(false)
      {
        :ancestors          => mod.ancestors.reverse,
        :constants          => mod.constants.sort,
        :instance_methods   => methods.sort,
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
      new(ObjectSpace._id2ref(object_id))
    end

    # Return a new ObjectInfo instance for the given object.
    def self.for(object)
      new(object)
    end

    attr_reader :info
    
    def initialize(obj)
      @info = { }
      @info[:object_id]          = obj.object_id
      @info[:class]              = obj.class.name
      @info[:inspect]            = obj.inspect
      @info[:instance_variables] = []
      @info[:enumerated]         = []
      @info[:enumerated_size]    = obj.respond_to?(:size) ? obj.size : nil

      obj.instance_variables.each do |iv|
        val = obj.instance_variable_get(iv)
        @info[:instance_variables] << [iv, val.inspect, val.object_id]
      end

      limit = 10
      case obj
      when Enumerable, Array
        obj.each_with_index do |o,i|
          if i > limit
            @info[:enumerated] << '...'
            break
          else
            @info[:enumerated] << o.inspect
          end
        end

      when Hash
        obj.each_with_index do |pair, idx|
          if idx > limit
            @info[:enumerated] << ['', '...']
            break
          end
          @info[:enumerated] << [pair[0].to_s, pair[1].inspect]
        end
      end

    end

    def to_json(*args)
      @info.to_json
    end
  end
end
