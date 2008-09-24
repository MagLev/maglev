def test
  yield( [ ])
end
z = 99
test { | a, b, *c |
  unless z == 99
    raise 'ERROR'
  end
  z = z + 1
  unless a.equal?(nil) 
    raise 'ERROR'
  end
  unless b.equal?(nil) 
    raise 'ERROR'
  end
  unless c.length == 0
    raise 'ERROR'
  end
  unless z == 100
    raise 'ERROR'
  end
}

unless z == 100
  raise 'ERROR'
 end
true
