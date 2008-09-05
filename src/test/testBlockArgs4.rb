def test
  yield( 7 )
end
test { | a, *b |
  unless a == 7
    raise 'ERR'
  end
  unless b == [ ]
    raise 'ERR'
  end
}
true
