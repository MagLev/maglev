def test
  yield( [ 9 , 10, 11 , 12 ])
end
test { | a, *b |
  unless a == 9
    raise 'ERR'
  end
  unless b == [ 10, 11, 12]
    raise 'ERR'
  end
  # puts a
  # puts '---'
  # puts b
}
true
