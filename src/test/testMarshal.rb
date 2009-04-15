h = {'one' => {'two' => {'three' => 0}}}
b = Marshal.dump(h, 6);

hb = Marshal.load(b)
unless h == hb ; raise 'error' ; end
puts "Hash ok"

k = Class.new
a = 0
begin
  a = Marshal.dump(k)
rescue TypeError
  puts "ok a"
end

m = Module.new
b = 0
begin
  b = Marshal.dump(m)
rescue TypeError
  puts "ok b"
end


true
