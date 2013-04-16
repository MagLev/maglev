class Object
  primitive_nobridge '__perform_with_args', 'perform:withArguments:'
end

module SmalltalkBridge
  def const_missing(name)
    begin
      cls = __resolve_smalltalk_global(name)
      const_set(name, cls)
      if cls.is_a? Class
        cls.send(:include, MethodBridge)
        cls.send(:extend, MethodBridge)
      end
      cls
    rescue Exception
      super
    end
  end

  module MethodBridge
    def method_missing(method, *args, &block)
      begin
        args << block.__block if block
        __perform_with_args(method, args)
      rescue Exception
        super
      end
    end
  end
end
