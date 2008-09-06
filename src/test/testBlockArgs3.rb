def test
  yield( [ ])
end
test { | a, b, c |
  unless a.equal?(nil) 
    raise 'ERROR'
  end
  unless b.equal?(nil) 
    raise 'ERROR'
  end
  unless c.equal?(nil) 
    raise 'ERROR'
  end
}
true
