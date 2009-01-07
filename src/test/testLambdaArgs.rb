def raise_err(a)
  puts a.inspect
  nil.pause
end

def testOne
  # per MRI 1.8.6, one arg lambdas to not check arg count
  t = 0
  p = lambda { | a |;  t = a ; }
  p.call(5)
  unless t == 5 ; raise_err(t); end
  arr = [50]
  p.call(*arr)
  unless t == 50 ; raise_err(t); end
  p.call(9,10)
  unless t == [9,10] ; raise_err(t); end
end

def testZero
  # per MRI 1.8.6, zero arg lambdas to not check arg count
  t = 0
  p = lambda { t = t + 10 ; }
  p.call
  unless t == 10 ; raise_err(t); end
  p.call(5)
  unless t == 20 ; raise_err(t); end
  p.call(5,6) 
  unless t == 30 ; raise_err(t); end
end

def testTwo
  p = lambda { |a,b| [a,b] }
  e = 0
  begin
    p.call
  rescue ArgumentError
    e = 1
  end
  unless e == 1 ; raise_err(e); end;
  e = 0
  begin
    p.call(3)
  rescue ArgumentError
    e = 1
  end
  unless e == 1 ; raise_err(e); end;
  r = p.call(9,10)
  unless r == [9,10] ; raise_err(r); end;
  e = 0
  begin
    p.call(3,4,5)
  rescue ArgumentError
    e = 1
  end
  unless e == 1 ; raise_err(e); end;
end

testOne
testZero
testTwo
true
