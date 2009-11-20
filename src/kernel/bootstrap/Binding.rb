class Binding
  # Binding is identically the Smalltalk class  RubyBinding
 
  class_primitive_nobridge '__build_names', '_bindingInfo:'

  def initialize( binding_context , obj, blk )
    info = Binding.__build_names( binding_context )
    @staticLink = info[0]   # staticLink may be nil
    @names = info[1]
    @selfObj = obj 
    @block = blk
    @forModuleEval = false
    # if this code changed see also RubyBinding>>ctx:mainSelf: 
  end

  def block
    @block
  end

end
