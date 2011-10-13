# test for Trac465
class Trac465
  def testa
    na = 0
    for nb in 2..4
      na += 1
      if na == 2
        break
      end
    end
    unless na == 2 ; raise 'error' ; end
    puts "testA ok"
  end

  def testb
    a = [ 5, 6]
    b = 0
    x = a.each { | o |
         b += 10
         break 45
       }
    unless b == 10 ; raise 'error' ; end
    unless x == 45 ; raise 'error' ; end
    puts "testB ok"
  end

  def m1
    yield
    :m1
  end

  def m2(pr)
    y = pr.call
    unless y == 37 ; raise 'error' ; end
    :m2
  end

  def testc
          # break should normal-return from block with value
    x = m2(proc { break 37; 'ab' } )
    raise "Expecting :m2 but got #{x} for break in block" unless x == :m2
    puts "testC ok"
  end
  def teste
          # break should normal-return from block with value
    x = m2(lambda { break 37; 'ab' } )
    raise "Expecting :m2 but got #{x} for break in block" unless x == :m2
    puts "testE ok"
  end

  def testd
    x = m1 { break 44 }   # break should return-to-home with value
    raise "Expecting 44  but got #{x} for break in proc"  unless x == 44
    puts "testD ok"
  end
end

o = Trac465.new
o.testa
o.testb
o.testc
o.testd
o.teste

# The original test case from Markus.
# This test case passes if it doesn't throw an Exception.
def g
     yield
     end

yy = g { break 33 }
unless yy == 33 ; raise 'error'; end
puts "OK"
true
#################### Trac Info
# ID:         465
# Summary:    Breaking out of a user defined generator
# Changetime: 2009-09-25 19:04:18+00:00
###

#  Breaking out of a user-written generator fails with the error
#  
#  {{{
#  ERROR 2023, break, Error, true
#  }}}
#  
#  Minimal failure case:
#  
#  {{{
#  def g
#      yield
#      end
#  
#  g { break }
#  }}}
#  
#  