
def test(a)
  if block_given?
    unless $HaveBlk == 1 
      raise 'ERR'
    end
    yield( [ 9 , 10, 11 , 12 ])
  else
    unless $HaveBlk == 0
      raise 'ERR'
    end
  end
end

$HaveBlk = 1

test (1){ | a, *b |
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

$HaveBlk = 0
test(nil)

true
