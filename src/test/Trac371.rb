# Distilled from rdoc:
class Foo
  "foo" =~ /(foo)/
  X = $1
end

unless Foo::X == 'foo' ; raise 'error' ; end;
true
