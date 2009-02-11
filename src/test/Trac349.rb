Node = Struct.new(:tag, :value)
class Node
  def foo
    349
  end
end
r = Node.new.foo 
unless r == 349 ; raise 'error'; end
true
