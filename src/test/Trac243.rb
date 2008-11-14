# ##### Trac # 243#######################################################
#
# The following should make the methods declared in the module to be module
# methods, but it doesn't. This example comes from Sinatra.

module Foo
  extend self
  def bar
    puts "bar called with #{self}"
  end
end

Foo.bar

