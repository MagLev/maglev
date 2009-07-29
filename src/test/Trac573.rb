class Foo
  def initialize(v=10)
    @foo = v
  end
end

f = Foo.new("hello")
f.instance_variable_set(:@animal, 'dog')

i = f.inspect

p f

raise 'missing @foo variable'    unless i =~ /@foo="hello"/
raise 'missing @animal variable' unless i =~ /@animal="dog"/
