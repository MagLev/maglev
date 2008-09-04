class TestSend

  def methZero
    puts "methZerook"
  end

  def meth2(*args)
    # puts args.class
    # puts args.length
    # puts args
    unless args.length == 2 
      raise 'ERR'
    end
    unless args[0] == 88
      raise 'ERR'
    end
    unless args[1] == 99
      raise 'ERR'
    end
    puts "meth2ok"
  end
  def meth2arr(*args)
    # puts args.class
    # puts args.length
    # puts args
    unless args.length == 1 
      raise 'ERR'
    end
    a = args[0]
    unless a.length == 2 
      raise 'ERR'
    end
    unless a[0] == 88
      raise 'ERR'
    end
    unless a[1] == 99
      raise 'ERR'
    end
    puts "meth2arrok"
  end

  def doSend2arr(*args)
     send :meth2arr , *args
     puts "send2ok"
  end
  def doSend2b(aSym, *args)
     send aSym, *args
     puts "send2ok"
  end
end

o = TestSend.new
o.send :methZero
o.send :meth2 , 88, 99
a = [88,99]
o.send :meth2arr , a
o.doSend2arr(a)
o.doSend2b( :meth2arr , a )
true
