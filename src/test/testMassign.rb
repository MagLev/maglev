def raiseErr
  raise 'ERR'
end

 a , b  = 1, 2, 3
 unless a == 1 then raiseErr end
 unless b == 2 then raiseErr end
 c, *d  = 4, 5, 6
 unless c == 4 then raiseErr end
 unless d == [ 5, 6 ] then raiseErr end

 # TODO , more test cases from p97..100 of OReilly Flanagan & Mats book"

 true
