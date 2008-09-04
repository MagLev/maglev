def test
  yield( [ 9 , 10, 11 , 12 ])
end
test { | a, b, c |
  puts a.class
  puts a
  puts b.class
  puts b
  puts c.class
  puts c
}
