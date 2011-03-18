# Methods to get access to method source code.

class Module
  # Return the source code for a method.
  #
  # @param [String,Symbol] method_name  The name of the method
  # @param [Boolean] instance_method If true, look for an instance method
  #         named +method_name+, otherwise look in receiver's singleton
  #         class for the method.
  # @return [GsNMethod] A non-bridge compiled method 
  #
  def gs_method_for(method_name, instance_method=true)
    meth = nil
    begin
      mname = method_name.to_sym
      target = instance_method ? self : self.__singleton_class
      meth = target.__gs_method(mname)
    rescue => e
      raise NameError, "Error trying to get #{instance_method ? "instance" : "class"} method source for class #{self.name} method #{method_name.inspect}: #{e.to_s}"
    end
  end
end
