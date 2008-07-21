# 
# Benchmark blocks of code from ruby-benchmark-suite/core-library
#

require 'benchmark'

puts 'Starting benchmarks...'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_app_strconcat '){
    i=0
    while i<500000
      "#{1+1} #{1+1} #{1+1}"
      i+=1
    end    
  }
  
  bmr.report('bm_so_array '){
    n = 9000 # Integer(ARGV.shift || 1)
    
    x = Array.new(n)
    y = Array.new(n, 0)
    q = []
    
    n.times{|bi|
      x[bi] = bi + 1
    }
    
    (0 .. 999).each do |e|
      (n-1).step(0,-1) do |bi|
        y[bi] += x.at(bi)
      end
    end
  }

  bmr.report('bm_so_concatenate '){
    STUFF = "hello\n"
    i=0
    while i<10
      i+=1
      hello = ''
      400000.times do |e|
        hello << STUFF
      end
    end
    
  }
  
  # require 'test/benchmark_so_count_words FAILS
  
  bmr.report('bm_so_exception '){
    $HI = 0
    $LO = 0
    NUM = 250000 # Integer(ARGV[0] || 1)
      
      
    class Lo_Exception < Exception
      def initialize(num)
        @value = num
      end
    end
      
    class Hi_Exception < Exception
      def initialize(num)
        @value = num
      end
    end
      
    def some_function(num)
      begin
        hi_function(num)
      rescue
        print "We shouldn't get here, exception is: #{$!.type}\n"
      end
    end
      
    def hi_function(num)
      begin
        lo_function(num)
      rescue Hi_Exception
        $HI = $HI + 1
      end
    end
      
    def lo_function(num)
      begin
        blowup(num)
      rescue Lo_Exception
        $LO = $LO + 1
      end
    end
      
    def blowup(num)
      if num % 2 == 0
        raise Lo_Exception.new(num)
      else
        raise Hi_Exception.new(num)
      end
    end
      
      
    i = 1
    max = NUM+1
    while i < max
      i+=1
      some_function(i+1)
    end      
      
  }

  bmr.report('bm_so_lists '){
    NUM = 100
    SIZE = 10000
  
    def test_lists()
      # create a list of integers (Li1) from 1 to SIZE
      li1 = (1..SIZE).to_a
      # copy the list to li2 (not by individual items)
      li2 = li1.dup
      # remove each individual item from left side of li2 and
      # append to right side of li3 (preserving order)
      li3 = Array.new
      while (not li2.empty?)
        li3.push(li2.shift)
      end
      # li2 must now be empty
      # remove each individual item from right side of li3 and
      # append to right side of li2 (reversing list)
      while (not li3.empty?)
        li2.push(li3.pop)
      end
      # li3 must now be empty
      # reverse li1 in place
      li1.reverse!
      # check that first item is now SIZE
      if li1[0] != SIZE then
        p "not SIZE"
        0
      else
        # compare li1 and li2 for equality
        if li1 != li2 then
          return(0)
        else
          # return the length of the list
          li1.length
        end
      end
    end
  
    i = 0
    while i<NUM
      i+=1
      result = test_lists()
    end
  
    result    
  }
  
  bmr.report('bm_so_matrix '){
    n = 60 #Integer(ARGV.shift || 1)

    size = 30

    def mkmatrix(rows, cols)
        count = 1
        mx = Array.new(rows)
        (0 .. (rows - 1)).each do |bi|
            row = Array.new(cols, 0)
            (0 .. (cols - 1)).each do |j|
                row[j] = count
                count += 1
            end
            mx[bi] = row
        end
        mx
    end

    def mmult(rows, cols, m1, m2)
        m3 = Array.new(rows)
        (0 .. (rows - 1)).each do |bi|
            row = Array.new(cols, 0)
            (0 .. (cols - 1)).each do |j|
                val = 0
                (0 .. (cols - 1)).each do |k|
                    val += m1.at(bi).at(k) * m2.at(k).at(j)
                end
                row[j] = val
            end
            m3[bi] = row
        end
        m3
    end

    m1 = mkmatrix(size, size)
    m2 = mkmatrix(size, size)
    mm = Array.new
    n.times do
        mm = mmult(size, size, m1, m2)
    end
    # puts "#{mm[0][0]} #{mm[2][3]} #{mm[3][2]} #{mm[4][4]}"
  }

  bmr.report('bm_vm2_array '){
    i=0
    while i<6000000 # benchmark loop 2
      i+=1
      a = [1,2,3,4,5,6,7,8,9,10]
    end
  }

  bmr.report('bm_vm2_regexp '){
    i=0
    str = 'xxxhogexxx'
    while i<6000000 # benchmark loop 2
      /hoge/ =~ str
      i+=1
    end 
  }  
 
  # FAILS - Thread lib not implemented
  # bmr.report('bm_vm3_thread_create_join  '){
  #   i=0
  #   while i<1000 # benchmark loop 3
  #     i+=1
  #     Thread.new{
  #     }.join
  #   end
  # }  

end
  
# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
