# This test passes by not blowing up...
require File.expand_path('simple', File.dirname(__FILE__))

module ModuleSpec
  module ::ModuleSpec
  end
end

test(ModuleSpec::ModuleSpec.class, Module, 'the test')

report
