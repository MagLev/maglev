def raise_err(a)
  puts a.inspect
  nil.pause
end
def trace(a)
  # puts a + 1
end

def testZero
  # per MRI 1.8.6, zero arg lambdas do not check arg count
  t = 0
  p = lambda { t = t + 10 ; }
  3.times {
    prev = t
    p.call
    unless t == prev + 10 ; raise_err(t); end
    trace(__LINE__)
  }
  3.times {
    prev = t
    p.call(5)
    unless t == prev + 10 ; raise_err(t); end
    trace(__LINE__)
  }
  3.times {
    prev = t
    p.call(8,6) 
    unless t == prev + 10 ; raise_err(t); end
    trace(__LINE__)
  }
end

def testOne
  # per MRI 1.8.6, one arg lambdas do not check arg count
  t = 0
  p = lambda { | a |;  t = a ; }
  3.times {
    t = 0
    p.call(5)
    unless t == 5 ; raise_err(t); end
    trace(__LINE__)
  }
  arr = [50]
  3.times {
    t = 0
    p.call(*arr)
    unless t == 50 ; raise_err(t); end
    trace(__LINE__)
  }
  3.times {
    t = 0
    p.call(9,10)
    unless t == [9,10] ; raise_err(t); end
    trace(__LINE__)
  }
end

def testTwo
  p = lambda { |a,b| [a,b] }
  2.times {
    e = 0
    begin
      p.call
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
  2.times {
    e = 0
    trace(__LINE__)
    begin
      p.call(3)
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
  3.times {
    r = p.call(9,10)
    unless r == [9,10] ; raise_err(r); end;
    trace(__LINE__)
  }
  2.times {
    e = 0
    begin
      p.call(3,4,5)
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
end

def testThree
  p = lambda { |a,b,c| [a,b,c] }
  2.times {
    e = 0
    begin
      p.call
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
  2.times {
    e = 0
    begin
      p.call(3)
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
  2.times {
    e = 0
    begin
      p.call(3,4)
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
  3.times {
    r = p.call(3,4,5)
    unless r == [3,4,5] ; raise_err(r); end
    trace(__LINE__)
  }
  2.times {
    e = 0
    begin
      p.call(3,4,5,6)
    rescue ArgumentError
      e = 1
    end
    unless e == 1 ; raise_err(e); end;
    trace(__LINE__)
  }
end

testOne()
testZero()
testTwo()
testThree()
true
