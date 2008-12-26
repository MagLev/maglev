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
true
