# This test passes by not blowing up...
require File.expand_path('simple', File.dirname(__FILE__))

module ModuleSpec
  module ::ModuleSpec
  end
end

# test edited to conform to fix of Trac 672
x = nil
begin
  mm = ModuleSpec::ModuleSpec.class
  raise 'error, ModuleSpec::ModuleSpec should not be defined'
rescue NameError
  # ok
  x = 260
end
unless x == 260 ; raise 'error'; end
true
