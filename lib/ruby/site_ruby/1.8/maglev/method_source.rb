# Methods to get access to method source code.

class Module
  # Return the source code for a method.
  #
  # @param [String,Symbol] method_name  The name of the method
  # @param [Boolean] instance_method If true, look for an instance method
  #         named +method_name+, otherwise look in receiver's singleton
  #         class for the method.
  # @return [String] The method source
  #
  def method_source_for(method_name, instance_method=true)
    src = nil
    begin
      mname = method_name.to_sym
      target = instance_method ? self : self.__singleton_class
      m = target.instance_method(mname)
      src = m.__nonbridge_meth.__source_string
    rescue => e
      raise NameError, "Error trying to get #{instance_method ? "instance" : "class"} method source for class #{self.name} method #{method_name.inspect}: #{e.to_s}"
    end
    src
  end
end
