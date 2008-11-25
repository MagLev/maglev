
# Test class creation via metaprogramming

begin
  Klass = Class.new(Object)
  o1 = Klass.new   

  klass = Class.new(Object)
  o2 = klass.new    

  $g263 = 0
  klass = Class.new(Object) do
     attr_accessor :foo
     def bar(v)
       $g263 = v
     end
   end
  
  o3 = klass.new
  o3.bar(99)
  unless $g263 == 99 ; raise 'ERR' ; end

  o3.foo = "foo"
  unless o3.foo == 'foo' ; raise 'ERR' ; end
end
true
