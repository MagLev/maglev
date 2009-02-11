def raise_err(a)
  puts a.inspect 
  nil.pause  
end
def trace(a)
  # puts a + 1
end

# need a repeat loop to cover the fast for second time
#   through invoking a block
def y_zeroArg
  3.times {
    yield
  }
end

def y_zeroArgErr
  2.times{
    e = 0
    begin
      yield
    rescue ArgumentError
      e = 1 #expect wrong num args
    end
    unless e == 1 ; raise_err(e); end
  }
end

def y_oneArg
  3.times {
    yield(8)
  }
end
def y_oneArgErr
  2.times{
    e = 0
    begin
      yield(8)
    rescue ArgumentError
      e = 1 #expect wrong num args
    end
    unless e == 1 ; raise_err(e); end
  }
end
def y_twoArgs
  3.times {
    yield(9, 10)
  }
end
def y_twoArgsErr
  2.times{
    e = 0
    begin
      yield(8,9)
    rescue ArgumentError
      e = 1 #expect wrong num args
    end
    unless e == 1 ; raise_err(e); end
  }
end
def y_threeArgs
  3.times {
    yield(8, 9, 10)
  }
end
def y_threeArgsErr
  2.times{
    e = 0
    begin
      yield(1,2,3)
    rescue ArgumentError
      e = 1 #expect wrong num args
    end
    unless e == 1 ; raise_err(e); end
  }
end
def y_emptyArr
  3.times {
   yield( [] )
  }
end
def y_emptyArrArr
  3.times {
    yield( [[]] ) 
  }
end
def y_arrayFour
  3.times {
    yield( [ 9 , 10, 11 , 12 ] )
  }
end

def y_arrayTwo
  3.times {
   yield( [ 6, 7 ])
  }
end

def y_starTwo
  3.times {
    yield( *[6, 7])
  }
end

def y_starOne(*args)
  3.times {
    yield *args
  }
end

# --------
# one arg

y_oneArg { | a |
    unless a == 8 ; raise_err(a) ; end
    trace(__LINE__)
  }

y_zeroArg { | a |
    unless a == nil ; raise_err(a) ; end;
    trace(__LINE__)
  }

y_oneArg { | a, b |
    x = [a,b]
    unless x == [8,nil] ; raise_err(x) ; end
    trace(__LINE__)
  }

y_twoArgs { | a |
  unless a == [9, 10] ; raise_err(a); end
    trace(__LINE__)
}

y_zeroArg { | a, b |
  x = [a,b]
  unless x == [nil,nil]; raise_err(x); end
    trace(__LINE__)
}

y_twoArgs { | a, b |
  x = [a,b]
  unless x == [9, 10] ; raise_err(x); end
    trace(__LINE__)
}

y_twoArgs { | a, b ,c |
  x = [a,b,c]
  unless x = [9, 10, nil] ; raise_err(x); end
    trace(__LINE__)
}

y_zeroArg { | a, b, c|
  x = [a,b,c]
  unless x == [nil, nil, nil] ; raise_err(x); end
    trace(__LINE__)
}

y_oneArg { | a, b, c|
  x = [a,b,c]
  unless x == [8, nil, nil] ; raise_err(x); end
    trace(__LINE__)
}

y_threeArgs { | a, b, c|
  x = [a,b,c]
  unless  x == [8, 9, 10] ; raise_err(x); end
    trace(__LINE__)
} 

y_threeArgs { | a, b|
  x = [a,b]
  unless  x == [8, 9] ; raise_err(x); end
    trace(__LINE__)
} 

y_threeArgs { | a|
  unless  a == [8, 9, 10] ; raise_err(a); end
   trace(__LINE__)
} 

$agl = 0
y_threeArgs { |*|   # coverage for lastStar logic for block with zero args
  $agl = 3
}
unless $agl == 3 ; raise_err($agl); end

true
