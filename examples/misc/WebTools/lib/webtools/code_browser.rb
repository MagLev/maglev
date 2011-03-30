require 'webtools/ruby'

module WebTools
  # The CodeBrowser is a ViewModel for MagLev code browsing.  It keeps the
  # entire state of the UI and returns structured data for use by a UI.
  # For the first pass, we do not cache anything.
  class CodeBrowser
    def self.class_and_module_list
      puts "#{self}.class_and_module_list()"
      { 'classNames' => WebTools::Ruby.class_and_module_names }
    end

    def select_module(module_name)
      puts "#{self}.select_module(#{module_name.inspect})"
      # At some point, if we need to cache individual classes/modules, or
      # if the state becomes complex, we could introduce a ModuleInfo class
      # to store the currently selected class/module.  Until then, this
      # suffices.
      mod = Ruby.find_in_namespace(module_name)
      @selected_class = module_name
      @constants = mod.constants.sort
      @class_methods = Ruby.module_fns_for(mod)
      @instance_methods = mod.instance_methods(false).sort
      state
    end

    def state
      { :selected_class   => @selected_class   || nil,
        :constants        => @constants        || [],
        :instance_methods => @instance_methods || [],
        :module_methods   => @module_methods   || [] }
    end
  end
end
