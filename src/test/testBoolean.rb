def raiseErr
  raise 'ERR'
end

 unless true.class.equal?(TrueClass) then raiseErr end
 unless false.class.equal?(FalseClass) then raiseErr end

 a = true ^ true
 unless a.equal?(false) then raiseErr end
 a = true ^ false
 unless a.equal?(true) then raiseErr end
 a = false ^ true
 unless a.equal?(true) then raiseErr end
 a = false ^ false
 unless a.equal?(false) then raiseErr end

 a = true | true
 unless a.equal?(true) then raiseErr end
 a = true | false
 unless a.equal?(true) then raiseErr end
 a = false | true
 unless a.equal?(true) then raiseErr end
 a = false | false
 unless a.equal?(false) then raiseErr end

 a = true & true
 unless a.equal?(true) then raiseErr end
 a = true & false
 unless a.equal?(false) then raiseErr end
 a = false & true
 unless a.equal?(false) then raiseErr end
 a = false & false
 unless a.equal?(false) then raiseErr end

# TODO add tests for  nil OP aBoolean
#                     aBoolean OP nil
#  for aBoolean   true or  false
#  for OP  one of  ^ | &

 true
