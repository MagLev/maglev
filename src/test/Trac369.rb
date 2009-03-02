# For Kernel#sprintf, MRI converts String arguments for numeric formats
# first, i.e., "%d" % ["123"], first converts "123" to numeric, then passes
# it to the underlying sprintf.
aa = []
%w(b d e E f g G i o u x X).each do |l|
  str = "%#{l}" % ["123"]
  aa << str
end

unless aa ==  [ "1111011", "123", "1.230000e+02", "1.230000E+02",
	"123.000000", "123", "123", "123", "173", "123", "7b", "7B"] 
   raise 'error' 
end
true
