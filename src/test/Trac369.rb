# For Kernel#sprintf, MRI converts String arguments for numeric formats
# first, i.e., "%d" % ["123"], first converts "123" to numeric, then passes
# it to the underlying sprintf.
%w(b d e E f g G i o u x X).each do |l|
  result = "%#{l}" % ["123"]
  puts "For #{l}: #{result}"
end

true
