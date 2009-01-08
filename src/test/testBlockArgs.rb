
T175 = true  # set to false with Ticket 175 completely fixed

def raise_err(a)
  puts a.inspect 
  nil.pause  
end
def trace(linenum)
  # puts linenum  + 1
end

def y_oneArg(x)
    yield(x)
end
def y_twoArgs
    yield(9, 10)
end
def y_threeArgs
    yield(8, 9, 10)
end
def y_emptyArr
   yield( [] )
end
def y_emptyArrArr
    yield( [[]] ) 
end
def y_arrayFour
    yield( [ 9 , 10, 11 , 12 ] )
end

def y_arrayTwo
   yield( [ 6, 7 ])
end

def y_starTwo
    yield( *[6, 7])
end

def y_starOne(*args)
    yield *args
end

# --------
# one arg

y_oneArg(5) { | a |
    unless a == 5 ; raise_err(a) ; end
    trace(__LINE__)
  }
y_oneArg([5]) { | a |
    unless a == [5] ; raise_err(a) ; end
    trace(__LINE__)
  }

y_twoArgs { | a |
  unless a == [9, 10] ; raise_err(a); end
    trace(__LINE__)
}

y_starOne(5) { | a |
   unless a == 5 ; raise_err(a) ; end
    trace(__LINE__)
 }

y_starOne([5]) { | a |
   unless a == [5] ; raise_err(a) ; end
    trace(__LINE__)
 }


y_emptyArrArr { | a |
  if (T175)
    exp = []
  else
    exp = [[]]
  end
  unless a == exp ; raise_err(a) ; end  # xx
  trace(__LINE__)
}

y_arrayTwo { | a | 
   unless a == [6, 7] ; raise_err(a) ; end
   trace(__LINE__)
}

y_starOne([6,7]) { | a |
  unless a == [6, 7] ; raise_err(a) ; end
  trace(__LINE__)
}

  
arr = [ 89 ]
p = Proc.new { | a |
    unless a == 89 ; raise_err(a) ; end
    trace(__LINE__)
  }
p.call(*arr)

# ---------
# one star arg

y_oneArg(5) { | *a |
   unless a == [ 5 ] ; raise_err(a) ; end
   trace(__LINE__)
 }

y_starOne( [ 5 ] ) { | *a |
   unless a == [[ 5 ]] ; raise_err(a) ; end
   trace(__LINE__)
 } 


y_arrayTwo { | *a |
  if (T175)
    exp = [6,7]
  else
    exp = [[6,7]]
  end
  unless a == exp ; raise_err(a) ; end 
   trace(__LINE__)
}

y_starOne([6,7]) { | *a |
  unless a == [[6, 7]] ; raise_err(a) ; end
   trace(__LINE__)
}
y_starTwo { | *a |
  unless a == [6, 7] ; raise_err(a) ; end
   trace(__LINE__)
}


y_threeArgs { | *a |
   # BUG, we get [[ 8 , 9, 10 ]] ;
   # cannot fix until ParseTree gem fixed to distinguish
   #   def ma; yield(8,9);end
   #  from
   #   def ma; yield([8,9]);end
   #unless a == [ 8 , 9, 10 ] ; raise_err(a); end
   trace(__LINE__)
 } 

y_emptyArr { | *a |
   unless a == [[]] ; raise_err(a) ; end
   trace(__LINE__)
 }

y_emptyArrArr{ | *a |
  if (T175)
    exp = [[]]
  else
    exp = [[[]]]
  end
  unless a == exp ; raise_err(a) ; end 
   trace(__LINE__)
}

# 3 args passed with   yield *args
y_starOne(1, 2, 3) { | *a | 
   act = *a
   exp = [ 1 , 2, 3 ]
   unless act == exp ; raise_err(act) ; end
    trace(__LINE__)
 } 

# --------
# one arg passed to  a, *b

y_oneArg(8) { | a, *b |
   act = [ a, b ]
   exp = [ 8, [ ] ]
   unless act == exp ; raise_err(act); end
    trace(__LINE__)
 }

# two args passed to  a, *b
#
y_twoArgs { | a, *b |
   act = [ a, b ]
   exp = [ 9, [10 ] ]
   unless act == exp ; raise_err(act); end
   trace(__LINE__)
 }

y_emptyArr { | a, *b |
   act = [ a, b ] 
   exp = [ nil, [] ]
   unless act == exp ; raise_err(act); end
   trace(__LINE__)
 }

y_emptyArrArr{ | a, *b |
   act = [ a, b ]
   if (T175)
     exp = [nil, []]  
   else
     exp = [[], []]  
   end
   unless act == exp ; raise_err(act); end  
   trace(__LINE__)
 }

# array of 4 elems passed to a,*b
#
y_arrayFour { | a, *b |
   act = [ a, b ]
   exp = [ 9, [10 , 11, 12] ]
   unless act == exp ; raise_err(act); end
   trace(__LINE__)
 }

y_arrayTwo { | a, *b | 
  act =  [a, b]  # ok
  unless act == [6, [7]] ; raise_err(act); end
   trace(__LINE__)
}

y_starTwo { | a, *b | 
  act =  [a, b]  # ok
  unless act == [6, [7]] ; raise_err(act); end
   trace(__LINE__)
}

y_starOne([6,7]) {  | a, *b |  
  act = [a, b]	 # ok
  unless act == [[6, 7], []] ; raise_err(act); end
   trace(__LINE__)
}



# ----------
# empty array passed to   a,b,c

y_emptyArr {  | a, b, c |
    act = [ a , b , c]
    exp = [ nil, nil, nil ]
    unless act == exp ; raise_err(act); end
   trace(__LINE__)
  }

y_emptyArrArr { |a,b,c|
   act = [ a , b , c]
   if (T175) 
     exp = [ nil, nil, nil ]
   else
     exp = [ [], nil, nil ]
   end
   unless act == exp ; raise_err(act); end
   trace(__LINE__)
  }

y_arrayTwo { | a,b, c| 
  act = [a,b,c]
  unless act == [6, 7, nil] ; raise_err(act); end
   trace(__LINE__)
 }

y_starTwo { | a,b, c| 
  act = [a,b,c]
  unless act == [6, 7, nil] ; raise_err(act); end
   trace(__LINE__)
}

y_starOne([6,7]) { | a,b, c| 
  act = [a,b,c]
  unless act == [[6, 7], nil, nil] ; raise_err(act); end
   trace(__LINE__)
}
 
# array of 4 elems passed to a,b,c , extra args thrown away
#
y_arrayFour { | a, b, c |
    act = [ a , b , c]
    exp = [ 9, 10, 11 ]
    unless act == exp ; raise_err(act); end
   trace(__LINE__)
  }

# -------- 
#   block declaring  a, b, *c

y_emptyArr { | a, b, *c |
   act = [ a, b, c ]
   exp = [ nil, nil, [] ]
   unless act == exp ; raise_err(act) ; end
   trace(__LINE__)
 }


y_emptyArrArr { |a, b, *c|
   act = [ a, b, c ]
   if (T175)
     exp = [ nil, nil, [] ]
   else
     exp = [ [], nil, [] ]
   end
   unless act == exp ; raise_err(act) ; end
   trace(__LINE__)
 }

y_arrayTwo { | a,b, *c| 
  act = [a,b,c]
  unless act == [6, 7, []] ; raise_err(act) ; end
   trace(__LINE__)
}
  
y_starTwo { | a,b, *c|
  act = [a,b,c]
  unless act == [6, 7, []] ; raise_err(act) ; end
   trace(__LINE__)
} 

y_starOne([6,7]) { | a,b, *c|
  act = [a,b,c]
  unless act == [[6, 7], nil, []] ; raise_err(act) ; end
   trace(__LINE__)
} 
true
