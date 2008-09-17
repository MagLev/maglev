class TestWhile
  def tstBreakValue
    n = 0
    loopVal = 97090
    loopVal = while n <= 6
      n = n + 1
      if (n == 4 ) 
        break 123
      end
    end 
    raise "ERROR" unless loopVal == 123
    raise "ERROR" unless n == 4
  end

  def tstBreakNoValue
    n = 0
    loopVal = 97090
    loopVal = while n <= 6
      n = n + 1
      if (n == 4 ) 
        # puts "break"
        break 
      end
    end 
    raise "ERROR" unless loopVal == nil
    raise "ERROR" unless n == 4
  end

  def tstNext
    n = 1
    k = 0
    nxt = 0
    loopVal = 97090
    loopVal = while n <= 6
      n = n + 1
      if (n == 2)
        next nxt = nxt + 99
      end
      k = k + 1 
    end
    raise "ERROR" unless loopVal == nil
    raise "ERROR" unless nxt==99 
    raise "ERROR" unless k==5
  end

  def tstRedo
    n = 1
    k = 0
    loopVal = 97090
    loopVal = while n <= 6
      n = n + 1
      if (n == 2)
        redo
      end
      k = k + 1
    end 
    raise "ERROR" unless loopVal == nil
    raise "ERROR" unless k==5
  end

  def tstAll
    n = 0
    a = 0
    b = 0
    c = 0 
    d = 0
    loopVal = 97090
    loopVal = while n <= 8
      n = n + 1 
      a = a + 10 
      if (n == 2)
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
    raise "ERROR" unless a==60
    raise "ERROR" unless b==500
    raise "ERROR" unless c==4000
    raise "ERROR" unless d==30000
  end
end

t = TestWhile.new
t.tstBreakValue
t.tstBreakNoValue
t.tstNext
t.tstRedo
t.tstAll
puts "done"

true
