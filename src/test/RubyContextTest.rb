# This is just a smoke test to ensure that RubyContext.load and
# RubyContext.save are available to ruby.  It passes by not raising an
# exception.

# Don't actually call the methods, as that may mess things up...

[:load_context, :save_context].each do |m|
  raise "RubyContext.#{m} not present" unless RubyContext.respond_to? m
end
