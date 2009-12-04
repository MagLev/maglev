class Struct
  # core/struct/element_reference_spec.rb wants an ArgumentError if too
  # many parameters are passed. Due to the manner in which the bridge
  # methods are installed, we define this method *before* the real
  # definition of [] (in common/struct.rb) and allow calls with only one
  # parameter to pass, and all others fail.
  def [](*vars)
    if vars.length._equal?(1)
      self[vars[0]]  # defined in common/struct.rb
    else
      raise ArgumentError, "wrong number of arguments (#{vars.length} for 1)"
    end
  end
end
