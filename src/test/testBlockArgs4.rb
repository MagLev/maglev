def test
  yield( 7 )
end
test { | a, *b |
  puts a.class
  puts a
  puts b.class
  puts b.length
}
