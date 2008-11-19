# ##### Trac # 243#######################################################
#
# This example comes from Sinatra.

$yy = 0 
module FooB
  include self
  def bar
    $yy = 88
  end
end

mb = FooB
mb.bar
unless $yy == 88
  raise 'ERR'
end

module Foo
  extend self
  def bar
    $yy = 99
  end
end

m = Foo
m.bar
unless $yy == 99
  raise 'ERR'
end
true
