class C
  TM = true
  FM = false  # next line must end with ?
  xx = TM ?
       5 : 6
  yy = FM ?  # white space after ?
       8 : 9
  unless xx == 5 ; raise 'error';end
  unless yy == 9 ; raise 'error';end
  puts "done 584"
end
true
