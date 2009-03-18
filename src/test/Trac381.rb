arx = /\A\/+\z/ 
a = (arx  =~ '')
brx = /\A\/*\z/ 
b = (brx =~ '') 
c = (/\A\/?\z/ =~ '') 
d = (/\A\/\z/ =~ '')
e = (/\A\\z/ =~ '')
 raise "Fail 1" unless a.equal?(nil)
 raise "Fail 2" unless b == 0
 raise "Fail 3" unless  c == 0
 raise "Fail 4" unless d.equal?(nil)
 raise "Fail 5" unless e.equal?(nil)
true
