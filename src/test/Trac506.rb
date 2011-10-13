
begin
  File.new('/xyzzy').read
  raise "EXPECTING AN EXCEPTION"
rescue Errno::ENOENT
  puts "A-OK"
rescue Exception => ex
  puts "Fail: got #{ex.class}"
end
#################### Trac Info
# ID:         506
# Summary:    File not raising the correct exception
# Changetime: 2009-04-27 19:08:05+00:00
###

#  File should raise a file not found exception when attempting to read a non-existent file.
#  
#  MBP:perf lattam$ irb
#  >> File.new('/favicon.ico').read
#  Errno::ENOENT: No such file or directory - /favicon.ico
#  	from (irb):1:in `initialize'
#  	from (irb):1:in `new'
#  	from (irb):1
#  >> quit
#  MBP:perf lattam$ maglev-irb
#  irb(main):001:0> File.new('/favicon.ico').read
#  Errno::ENOENT: Error,  nil 
#  	from /Users/lattam/maglev/MagLev-21583.MacOSX/bin/maglev-irb:24: in 'Object >> _compileFile'
#  
#  