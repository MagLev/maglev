module FFI
  # resolve these classes , so  parser sees FFI::CLibrary
  # as a class during ffi.rb, and dynamic constant definitions, if any,
  # will work.

  CLibrary = _resolve_smalltalk_global( :CLibrary )
  CFunction = _resolve_smalltalk_global( :CFunction )
  CByteArray = _resolve_smalltalk_global( :CByteArray )
  CPointer = _resolve_smalltalk_global( :CPointer )
end
