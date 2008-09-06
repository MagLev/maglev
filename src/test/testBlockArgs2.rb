def test
  yield( [ 9 , 10, 11 , 12 ])
end
test { | a, b, c |
  unless a == 9
    raise 'ERROR'
  end
  unless b == 10
    raise 'ERROR'
  end
  unless c == 11
    raise 'ERROR'
  end
}
true
