def test
  yield( 9, 10 )
end
test { | a, *b |
  puts a.class
  puts a
  puts b.class
  puts b.length
}
