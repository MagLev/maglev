class Foo
  module Bar ; end
  class Baz  ; end
end

bar_name = Foo::Bar.name
baz_name = Foo::Baz.name

raise "Bar: expect Foo::Bar actual: '#{bar_name}' " unless bar_name == "Foo::Bar"
raise "Baz: expect Foo::Baz actual: '#{baz_name}' " unless baz_name == "Foo::Baz"




