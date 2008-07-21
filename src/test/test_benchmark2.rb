# 
# Tests Benchmark.benchmark
#  

require 'benchmark'

capt = "      user     system      total        real\n"
fmt = "%10.6u %10.6y %10.6t %10.6r\n"

Benchmark.benchmark(" "*7 + capt, 7, fmt, ">total:", ">avg:") do |x|
    n = 1000
  	tf = x.report("for:")   { for i in 1..n; a = "1"; end }
		tt = x.report("times:") { n.times do   ; a = "1"; end }
		tu = x.report("upto:")  { 1.upto(n) do ; a = "1"; end }
    [tf+tt+tu, (tf+tt+tu)/3]
end

# Prevent saving this into RubyContext
Gemstone.abortTransaction rescue nil
