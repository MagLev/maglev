# coverage for Trac 887

a = Socket.const_get(:Constants)
b = a.name
unless b == 'Socket::Constants' ; raise 'fail'; end

d = File.const_get(:Constants)
e = d.name
unless e = 'File::Constants' ; raise 'fail' ; end
true
