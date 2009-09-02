# trac 463

class Trac463
   def testOne
     a = [ 5, 6 ]
     b = 0
     while not a.empty?
       b += 10
       next if a.shift
     end
     unless b == 20 ; raise 'error'; end
     puts "testOne ok"
   end

   def testTwo
     a = [ 5, 6 ]
     b = 0
     while not a.empty?
       begin
         b += 10
	 next if a.shift
       rescue ZeroDivisionError => err
	 p "This never happens"
       end
     end
     unless b == 20 ; raise 'error'; end
     puts "testTwo ok"
   end

  def testThree
    n = 0
    x = while true
      n += 1
      if n >= 1000
        break 95
      end
      begin
        if n >= 1000
          break 96
        end
        next
      rescue ZeroDivisionError => err
        p "This never happens"
      end
    end
    unless x == 95 ; raise 'error'; end
    puts "testThree ok"
  end
end

o = Trac463.new
o.testOne
# o.testTwo
o.testThree
puts "OK"
