class NilClass
 unless defined?(Maglev)
   def pause
   end
 end
end


$debugx = false

class C
  def initialize(*args)
    @ary = args
  end

  def docall(a, &block)
    block.call(a)
  end

  def _each(&block)
    n = 0
    ary = @ary
    lim = ary.size
    while n < lim
      num = n
      puts "start elem #{num}" if $debugx
      block.call(ary[n])
      n += 1
      puts "end elem #{num}" if $debugx
    end
  end
end

class T
  def self.test
    c = $cx
    c._each { |o| 
      $aa << o
      puts "val #{o}" if $debugx
      if o == 20
        puts "-- next"   if $debugx
        next
      end
      if o == 30
        puts "-- break"   if $debugx
        nil.pause  if $debugx
        break
      end
    }
    puts "===    End first iteration" if $debugx
  end

  def self.test2
    c = $cx
    c._each { |v|
      c.docall(v) { |o|
        $aa << o
	puts "val #{o}" if $debugx
	if o == 20
	  puts "-- next" if $debugx
	  next
	end
	if o == 30
	  puts "-- break" if $debugx
          nil.pause if $debugx
	  break
	end
      }
    }
  end
end
$aa = []
$cx = C.new(10,20,30,40)
T.test
T.test2
unless (ax = $aa) == [10,20,30, 10,20,30,40] ; raise 'fail'; end
true
