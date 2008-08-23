# I've tweaked this a bit from the raw Rubinius.
# BEGIN RUBINIUS: From rubinius rubinius/kernel/core/kernel.rb
module Type

  ##
  # Returns an object of given class. If given object already is one, it is
  # returned. Otherwise tries obj.meth and returns the result if it is of the
  # right kind. TypeErrors are raised if the conversion method fails or the
  # conversion result is wrong.
  #
  # Uses Type.obj_kind_of to bypass type check overrides.
  #
  # Equivalent to MRI's rb_convert_type().

  def self.coerce_to(obj, cls, meth)
    #return obj if obj.kind_of?(cls)
    return obj if obj.kind_of?(cls)   # GEMSTONE mod

    begin
      ret = obj.__send__(meth)
    rescue Exception => e
      raise TypeError, "Coercion error: #{obj.inspect}.#{meth} => #{cls} failed:\n" \
                       "(#{e.message})"
    end

    #return ret if ret.kind_of?(cls)
    return ret if ret.kind_of?(cls)   # GEMSTONE mod

    raise TypeError, "Coercion error: obj.#{meth} did NOT return a #{cls} (was #{ret.class})"
  end
end
# END RUBINIUS
