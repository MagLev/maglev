class TestFor
  # tests of for loops that will be inlined because
  #  they have Fixnum literals for start and/or end

  def tstBreakValue
    # tests for flip2 sexpression # note == higher precedence that ..
    v = 9
    if v == 0..(w=66) ; y = 3 ; else ; y = 4 ; end
    unless y == 4; raise 'err' ; end
    unless w.nil?; raise "err w=#{w.inspect}" ; end # note MRI gets w==nil
    v = 0
    if v == 0..(w=77) ; y = 3 ; else ; y = 4 ; end
    unless y == 3; raise 'err' ; end
    unless w == 77; raise 'err' ; end

    loopVal = 97090
    loopVal = for n in 0..6 
      if (n == 4 ) 
        break 123
      end
    end 
    raise "ERROR" unless loopVal == 123 # value from break
    raise "ERROR" unless n == 4
  end

  def tstBreakNoValue
    loopVal = 97090
    loopVal = for n in 0..6
      if (n == 4 ) 
        # puts "break"
        break 
      end
    end 
    raise "ERROR" unless loopVal.equal?(nil)
    raise "ERROR" unless n == 4
  end

  def tstNext
    k = 0
    nxt = 0
    loopVal = 97090
    loopVal = for n in 1..6
      if (n == 2)
        next nxt = nxt + 99
      end
      k = k + 1 
    end
    raise "ERROR" unless loopVal == (1..6)
    raise "ERROR" unless nxt==99 
    raise "ERROR" unless k==5
  end
  
  def tstRedo
    k = 0
    m = 0
    loopVal = 97090
    loopVal = for n in 1..6
      m = m + 1
      if (m == 2)
        redo
      end
      k = k + 1
    end 
    raise "ERROR" unless loopVal == (1..6)
    raise "ERROR" unless k==6
    raise "ERROR" unless m==7
  end

  def tstAll
    a = 0
    b = 0
    c = 0 
    d = 0
    m = 0
    loopVal = 97090
    loopVal = for n in 1..8
      m = m + 1
      a = a + 10 
      if (m == 2)
        redo
      end
      b = b + 100  
      if (n == 6)
        break 44
      end
      c = c + 1000
      if (n == 4)
        next
      end
      d = d + 10000
    end
    raise "ERROR" unless loopVal == 44
    raise "ERROR" unless n==6
    raise "ERROR" unless a==70
    raise "ERROR" unless b==600
    raise "ERROR" unless c==5000
    raise "ERROR" unless d==40000
  end
end

t = TestFor.new
t.tstBreakValue
t.tstBreakNoValue
t.tstNext
t.tstRedo
t.tstAll
puts "done"

true
