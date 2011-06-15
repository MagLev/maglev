class C
  define_method("xy:*&") do
    puts "in xy:&"
  end
  send(:define_method, ':') do
    puts "in colon"
  end
end

c = C.new
c.send('xy:*&')
c.send(':')

C.remove_method("xy:*&")  
true
