# This code generates the following error:
#   An attempt was made to evaluate the block or method anExecBlock with 1 arguments when 0 were expected. 
# 
 
str = ""; 1000.times{str << "x"}
raise "Faile" unless str.length == 1000
