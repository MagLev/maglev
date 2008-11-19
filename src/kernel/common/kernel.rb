# All the code in this file comes from rubinius common/kernel.rb.  But, not
# all of the code in common/kernel.rb is in here...
module Kernel
  def Float(obj)
    raise TypeError, "can't convert nil into Float" if obj.nil?

    if obj.is_a?(String)
      if obj !~ /^(\+|\-)?\d+$/ && obj !~ /^(\+|\-)?(\d_?)*\.(\d_?)+$/ && obj !~ /^[-+]?\d*\.?\d*e[-+]\d*\.?\d*/
        raise ArgumentError, "invalid value for Float(): #{obj.inspect}"
      end
    end

    Type.coerce_to(obj, Float, :to_f)
  end
  module_function :Float

# TODO: Fix
#   def Integer(obj)
#     if obj.is_a?(String)
#       if obj == ''
#         raise ArgumentError, "invalid value for Integer: (empty string)"
#       else
#         return obj.to_inum(0, true)
#       end
#     end
#     method = obj.respond_to?(:to_int) ? :to_int : :to_i
#     Type.coerce_to(obj, Integer, method)
#   end
#  module_function :Integer

  def Array(obj)
    if obj.respond_to?(:to_ary)
      Type.coerce_to(obj, Array, :to_ary)
    elsif obj.respond_to?(:to_a)
      Type.coerce_to(obj, Array, :to_a)
    else
      [obj]
    end
  end
 module_function :Array

  def String(obj)
    Type.coerce_to(obj, String, :to_s)
  end
  module_function :String

  ##
  # MRI uses a macro named NUM2DBL which has essentially the same semantics as
  # Float(), with the difference that it raises a TypeError and not a
  # ArgumentError. It is only used in a few places (in MRI and Rubinius).
  #--
  # If we can, we should probably get rid of this.

  def FloatValue(obj)
    begin
      Float(obj)
    rescue
      raise TypeError, 'no implicit conversion to float'
    end
  end
#  private :FloatValue   # TODO: uncomment
end
