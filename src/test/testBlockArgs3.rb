def test
  yield( [ ])
end
test { | a, b, c |
  unless a.equal?(nil) 
    raise 'ERR'
  end
  unless b.equal?(nil) 
    raise 'ERR'
  end
  unless c.equal?(nil) 
    raise 'ERR'
  end
}
true
