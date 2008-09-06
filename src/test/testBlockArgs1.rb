def test
  yield( 9, 10 )
end
test { | a, *b |
  unless a == 9
    raise 'ERR'
  end
  unless b == [ 10 ]
    raise 'ERR'
  end
}

# simple test of ranges
r = 1..3 
unless r.last == 3
  raise 'ERR'
end
a = r.to_a 
unless a == [1,2,3]
  raise 'ERR'
end
r = Range.new(1,5)
a = r.to_a 
unless a == [1,2,3,4,5]
  raise 'ERR'
end

true
