class TransientShortArray
  # TransientShortArray is used by the RubyParser .
  # it supports contigous in-memory instances up to 65Kbytes.
  
  class_primitive_nobridge '_with_shorts', '_withAllShorts:'
    # copies an Array

  primitive_nobridge '[]', '_rubyParserShortAt:'
    # zero values are translated to a nil result

  primitive_nobridge '[]=', '_rubyShortAt:put:'
end
