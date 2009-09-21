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
	 if a.shift
           next
         end
       rescue ZeroDivisionError => err
	 p "This never happens"
       end
     end
     unless b == 20 ; raise 'error'; end
     puts "testTwo ok"
   end

   def testTwoU
     a = [ 5, 6 ]
     b = 0
     begin
       begin
         b += 10
	 if a.shift
           next
         end
       rescue ZeroDivisionError => err
	 p "This never happens"
       end
     end while not a.empty?
     unless b == 20 ; raise 'error'; end
     puts "testTwoU ok"
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

  def testThreeU
    n = 0
    begin
      n += 1
      if n >= 4
        break 95
      end
      begin
        if n >= 4
          break 96
        end
        next
      rescue ZeroDivisionError => err
        p "This never happens"
      end
    end until not n <= 6
    unless n == 4
      puts "n = #{n}"
     raise 'error' 
    end
    puts "testThreeU ok"
  end


end

o = Trac463.new
o.testOne
o.testTwo
o.testThree
o.testTwoU
o.testThreeU
puts "OK"
true
