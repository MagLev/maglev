class Foo
  alias == eql?
end

Foo.new == Foo.new
