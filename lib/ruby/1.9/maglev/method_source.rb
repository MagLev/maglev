# Methods to get access to method source code.

class Module
  # Return the source code for a method.
  #
  # @param [String,Symbol] method_name  The name of the method
  # @param [Boolean] instance_method If true, look for an instance method
  #         named +method_name+, otherwise look in receiver's singleton
  #         class for the method.
  # @return [Array] Array of [String, String, Fixnum] method source, file name and line
  #
  def method_source(method_name, instance_method=true)
    gsnmeth = __gs_method(method_name, instance_method)
    src = gsnmeth.__source_string
    file,line = gsnmeth.__source_location
    [src,file,line]
  end
end
