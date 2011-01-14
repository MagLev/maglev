# See details in persisted_autoloads.rb
#
# Do NOT require foo, as it should be pre-loaded.

raise "Could not find module Foo: have you run persisted_autoloads.rb?" unless defined? :Foo

fb = Foo::Basic.new
p fb
