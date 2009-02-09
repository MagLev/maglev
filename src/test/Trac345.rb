module HTTPStatus
  eval "class Foo; end"
  puts const_get(:Foo)  # Should be HTTPStatus::Foo
end
