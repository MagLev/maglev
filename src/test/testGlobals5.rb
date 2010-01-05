class MA
 def initMe
   a = X
   a
 end
 X = 7
end
X = 5
cl = MA
o = MA.new
r = o.initMe 
unless r == 7
  raise 'ERROR'
end

# test defined?  for non-predefined global variables 
a1 = defined?($xx)
b = $xx
a2 = defined?($xx)
$xx = 99
a3 = defined?($xx)
z = $xx
unless (ax = [ a1, b, a2, a3, z ]) == [ nil, nil, nil, 'global-variable', 99]
  raise 'Error'
end

b1 = defined?($DEBUG)
b2 = $DEBUG
unless [ b1, b2 ] == [ 'global-variable' ,  false ]
  raise 'Error'
end

true
