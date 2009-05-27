# test of heredoc  parsing
a = <<HERE
abc
HERE
unless a == "abc\n" ; raise 'ERR'; end

b = <<-EOS, 'jkl'
abc
def
   EOS
unless b == ["abc\ndef\n", "jkl" ] ; raise 'ERR'; end

c = <<EA , 'jkl' , <<EB
abc
EA
def
EB
unless c == ["abc\n", "jkl", "def\n"] ; raise 'ERR'; end

puts "DONE"
true
