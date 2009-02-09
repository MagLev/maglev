some_constants = [
  "IO", "Binding", "TypeError", "Module", "RUBY_PLATFORM",
  "Kernel", "String" ]

some_constants.each do |cname|
  raise "Object Can't find constant #{cname}" unless Object.const_get cname
  raise "Module Can't find constant #{cname}" unless Module.const_get cname
end
