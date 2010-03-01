# multicharacter EOL indicators
#
f = File.open(__FILE__)
l = f.gets("open")  # =>  "f = File.open"

exp = "# multicharacter EOL indicators\n#\nf = File.open" 
unless l == exp ; raise 'error' ; end
puts "OK"
true
