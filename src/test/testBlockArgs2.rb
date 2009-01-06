def raise_err(a)
  puts a.inspect 
  nil.pause  # do not checkin
end
def trace(a)
  # puts a
end

def y_oneArg
  #1.times do | n |
    yield(8)
  #end
end
def y_twoArgs
  #1.times do | n |
    yield(9, 10)
  #end
end
def y_threeArgs
  #1.times do | n |
    yield(8, 9, 10)
  #end
end
def y_emptyArr
  #1.times do | n |
   yield( [] )
  #end
end
def y_emptyArrArr
  #1.times do | n |
    yield( [[]] ) 
  #end
end
def y_arrayFour
  #1.times do | n |
    yield( [ 9 , 10, 11 , 12 ] )
  #end
end

def y_arrayTwo
  #1.times do | n |
   yield( [ 6, 7 ])
  #end
end

def y_starTwo
  #1.times do | n |
    yield( *[6, 7])
  #end
end

def y_starOne(*args)
  #1.times do | n |
    yield *args
  #end
end

# --------
# one arg

y_oneArg { | a |
    unless a == 8 ; raise_err(a) ; end
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
true
