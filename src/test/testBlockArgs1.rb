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
unless a == [1,2,3]
  raise 'ERR'
end
r = Range.new(1,5)
a = r.to_a 
unless a == [1,2,3,4,5]
  raise 'ERR'
end

p = Proc.new { | a | 
   unless a.equal?(99)
     raise 'ERR'
   end
}
p[99]

q = Proc.new { | b |
  unless b.equal?(88)
     raise 'ERR'
  end
}
arr = [ 88 ]
q[*arr]

true
