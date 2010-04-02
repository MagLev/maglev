#  The small part of FFI used by the parser.
#  The full FFI is not loaded in environment 2
module FFI

  CByteArray =  __resolve_smalltalk_global( :CByteArray )
  class CByteArray
    # methods in Smalltalk CByteArray class needed by the string scanner
    class_primitive_nobridge 'with_string', '_vmOwnedWithAll:'

    primitive_nobridge '[]', '_rubyByteAt:'
    primitive_nobridge 'size', 'size'

  end
end
