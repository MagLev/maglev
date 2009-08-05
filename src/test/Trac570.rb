specs = [['Abc 10',   10] ]
specs.each do |y,|
  raise "y got #{y.inspect}" unless y == 'Abc 10'
end

#  adapted from  language/variables_spec.rb
# MRI gives different answers if you call the block
#  directly or as a lambda

 a = [ 42 , [42], [[42]], [42,55]]
 b = []
 c = []
 h = true
 a.each do |x,|
    b << x
 end
 unless b = [42, 42, [42], 42] ; raise 'error'; end
 # 
 f = lambda{|z,| z}
 a.each do | y |
   # Maglev gives same result as MRI, 
   # if  you pass enough args to satisify the dummy arg
   # otherwise you get too-few args error here
   x = f.call(y, 0)
   c << x
 end
 # note MRI gets c == [42, [42], [[42]], [42, 55]]
 unless c == [42, [42], [[42]], [42, 55]] ; raise 'error'; end

true
