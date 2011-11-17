module FFI
  # resolve these classes , so  parser sees FFI::CLibrary
  # as a class during ffi.rb, and dynamic constant definitions, if any,
  # will work.

  CLibrary = DynamicLibrary = __resolve_smalltalk_global( :CLibrary )
  CCallout = __resolve_smalltalk_global( :CCallout )
  CCallin =  __resolve_smalltalk_global(  :CCallin )
  Pointer =  __resolve_smalltalk_global( :CByteArray )
  CPointer = __resolve_smalltalk_global( :CPointer )
end
