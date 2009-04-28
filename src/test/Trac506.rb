
begin
  File.new('/xyzzy').read
  raise "EXPECTING AN EXCEPTION"
rescue Errno::ENOENT
  puts "A-OK"
rescue Exception => ex
  puts "Fail: got #{ex.class}"
end
