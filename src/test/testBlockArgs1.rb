def test
  yield( 9, 10 )
end
test { | a, *b |
  unless a == 9
    raise 'ERROR'
  end
  unless b == [ 10 ]
    raise 'ERROR'
  end
}

# simple test of ranges
r = 1..3 
unless r.last == 3
  raise 'ERROR'
end
a = r.to_a 
unless a = [1,2,3]
  raise 'ERROR'
end
r = Range.new(1,5)
a = r.to_a 
unless a = [1,2,3,4,5]
  raise 'ERROR'
end

true
