
module HTTPStatus
  eval "class Foo; def get; 'Trac345' ; end ; end"
  $a = Foo.new 
end

a = $a 
b = HTTPStatus::Foo.new
unless a.get == 'Trac345' ; raise 'error'; end 
unless b.get == 'Trac345' ; raise 'error'; end 
true
