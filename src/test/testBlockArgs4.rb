def test
  yield( 7 )
end
test { | a, *b |
  unless a == 7
    raise 'ERROR'
  end
  unless b == [ ]
    raise 'ERROR'
  end
}
true
