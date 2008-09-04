def test
  yield( [ 9 , 10, 11 , 12 ])
end
test { | a, b, c |
  unless a == 9
    raise 'ERR'
  end
  unless b == 10
    raise 'ERR'
  end
  unless c == 11
    raise 'ERR'
  end
}
true
