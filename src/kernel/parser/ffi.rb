# this file no longer used by parser, now that FFI is loaded
#   as part of the bootstrap classes for env 2.
module FFI

  class CByteArray
    # methods in Smalltalk CByteArray class needed by the string scanner
    class_primitive_nobridge 'with_string', 'withAll:'

    primitive_nobridge '[]', '_rubyByteAt:'
    primitive_nobridge 'size', 'size'

  end
end
