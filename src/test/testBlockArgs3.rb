def test
  yield( [ ])
end
test { | a, b, c |
  puts a.class
  puts a
  puts b.class
  puts b
  puts c.class
  puts c
}
