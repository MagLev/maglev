# 
# Benchmark blocks of code from ruby-benchmark-suite/core-features
#

require 'benchmark'

puts 'Starting benchmarks...'

Benchmark.bm(12) do |bmr|
 
  bmr.report('bm_app_answer '){
    def ack(m, n)
      if m == 0 then
        n + 1
      elsif n == 0 then
        ack(m - 1, 1)
      else
        ack(m - 1, ack(m, n - 1))
      end
    end
  
    def the_answer_to_life_the_universe_and_everything
      (ack(3,7).to_s.split(//).inject(0){|s,x| s+x.to_i}.to_s + "2" ).to_i
    end
  
    puts the_answer_to_life_the_universe_and_everything
  }
  
  bmr.report('bm_app_factorial '){
    def fact(n)
      if(n > 1)
        n * fact(n-1)
      else
        1
      end
    end

    5.times do 
      # puts fact(5000)
      fact(5000)
    end
  }

  bmr.report('bm_app_factorial2 '){
    def fact(n)
      if(n > 1)
        n * fact(n-1)
      else
        1
      end
    end

    5.times do 
      # puts fact(3000)
      fact(3000)
    end
  }
  
  bmr.report('bm_app_fib '){
    def fib(n)
      if n < 2
        n
      else
        fib(n-1) + fib(n-2)
      end
    end

    # 35.times {|n| puts fib(n) }
    35.times {|n| fib(n) }

  }

  bmr.report('bm_app_raise '){
    i=0
    while i<300000
      i+=1
      begin
        raise
      rescue
      end
    end
  }
  
  bmr.report('bm_app_tak '){
    def tak x, y, z
      unless y < x
        z
      else
        tak( tak(x-1, y, z),
          tak(y-1, z, x),
          tak(z-1, x, y))
      end
    end

    tak(18, 9, 0)    
  }
  
  bmr.report('bm_app_tarai '){
    def tarai( x, y, z )
      if x <= y
        then y
      else tarai(tarai(x-1, y, z),
          tarai(y-1, z, x),
          tarai(z-1, x, y))
      end
    end

    tarai(12, 6, 0)
  }
  
  bmr.report('bm_loop_times '){
    30000000.times{|e|}
  }
  
  bmr.report('bm_whileloop '){
    i = 0
    while i<30000000 # benchmark loop 1
      i+=1
    end  
  }
  
  bmr.report('bm_whileloop2 '){
    i=0
    while i<60000000 # benchmark loop 2
      i+=1
    end
  }
  
  bmr.report('bm_so_ackermann '){
    def ack(m, n)
      if m == 0 then
        n + 1
      elsif n == 0 then
        ack(m - 1, 1)
      else
        ack(m - 1, ack(m, n - 1))
      end
    end
    
    NUM = 9
    ack(3, NUM)
  }  
  
  bmr.report('bm_so_nested_loop '){
    n = 16 # Integer(ARGV.shift || 1)
    x = 0
    n.times do
      n.times do
        n.times do
          n.times do
            n.times do
              n.times do
                x += 1
              end
            end
          end
        end
      end
    end
  }
  
  bmr.report('bm_so_object '){
    class Toggle
      def initialize(start_state)
        @bool = start_state
      end
  
      def value
        @bool
      end
  
      def activate
        @bool = !@bool
        self
      end
    end
  
    class NthToggle < Toggle
      def initialize(start_state, max_counter)
        super start_state
        @count_max = max_counter
        @counter = 0
      end
  
      def activate
        @counter += 1
        if @counter >= @count_max
          @bool = !@bool
          @counter = 0
        end
        self
      end
    end
  
    n = 1500000 # (ARGV.shift || 1).to_i
  
    toggle = Toggle.new 1
    5.times do
      toggle.activate.value ? 'true' : 'false'
    end
    n.times do
      toggle = Toggle.new 1
    end
  
    ntoggle = NthToggle.new 1, 3
    8.times do
      ntoggle.activate.value ? 'true' : 'false'
    end
    n.times do
      ntoggle = NthToggle.new 1, 3
    end
  
  }

  bmr.report('bm_so_random '){
    IM = 139968.0
    IA = 3877.0
    IC = 29573.0
  
    $last = 42.0
  
    def gen_random(max)
      (max * ($last = ($last * IA + IC) % IM)) / IM
    end
  
    N = 1000000
  
    i=0
    while i<N
      i+=1
      gen_random(100.0)
    end
  }
  
  bmr.report('bm_startup '){
    1 + 1
  }  

  bmr.report('bm_vm1_block '){
    def m
      yield
    end

    i=0
    while i<30000000 # while loop 1
      i+=1
      m{
      }
    end
  }

  bmr.report('bm_vm1_const '){
    Const = 1

    i = 0
    while i<30000000 # while loop 1
      i+= 1
      j = Const
      k = Const
    end

  } 

  bmr.report('bm_vm1_ensure '){
    i=0
    while i<30000000 # benchmark loop 1
      i+=1
      begin
        begin
        ensure
        end
      ensure
      end
    end
  }  
  
  bmr.report('bm_vm1_length '){
    a = 'abc'
    b = [1, 2, 3]
    i=0
    while i<30000000 # while loop 1
      i+=1
      a.length
      b.length
    end
  }
    
  bmr.report('bm_vm1_rescue '){
    i=0
    while i<30000000 # while loop 1
      i+=1
      begin
      rescue
      end
    end    
  }

  bmr.report('bm_vm1_simplereturn '){
    def m
      return 1
    end
    i=0
    while i<30000000 # while loop 1
      i+=1
      m
    end
  }
  
  bmr.report('bm_vm1_swap '){
    a = 1
    b = 2
    i=0
    while i<30000000 # while loop 1
      i+=1
      a, b = b, a
    end
  }

  bmr.report('bm_vm2_method '){
    def m
      nil
    end

    i=0
    while i<6000000 # benchmark loop 2
      i+=1
      m; m; m; m; m; m; m; m;
    end    
  }
    
  bmr.report('bm_vm2_poly_method '){
    class C1
      def m
        1
      end
    end
    class C2
      def m
        2
      end
    end

    o1 = C1.new
    o2 = C2.new

    i=0
    while i<6000000 # benchmark loop 2
      o = (i % 2 == 0) ? o1 : o2
      o.m; o.m; o.m; o.m; o.m; o.m; o.m; o.m
      i+=1
    end
  }

  bmr.report('bm_vm2_poly_method_ov '){
    class C1
      def m
        1
      end
    end
    class C2
      def m
        2
      end
    end

    o1 = C1.new
    o2 = C2.new

    i=0
    while i<6000000 # benchmark loop 2
      o = (i % 2 == 0) ? o1 : o2
      #  o.m; o.m; o.m; o.m; o.m; o.m; o.m; o.m
      i+=1
    end
  }

  bmr.report('bm_vm2_proc '){
    def m &b
      b
    end

    pr = m{
      a = 1
    }

    i=0
    while i<6000000 # benchmark loop 2
      i+=1
      pr.call
    end    
  }

  bmr.report('bm_vm2_send '){
    class C
      def m
      end
    end

    o = C.new

    i=0
    while i<6000000 # benchmark loop 2
      i+=1
      o.__send__ :m
    end    
  }
   
  bmr.report('bm_vm2_super '){
    class C
      def m
        1
      end
    end

    class CC < C
      def m
        super()
      end
    end

    obj = CC.new

    i = 0
    while i<6000000 # benchmark loop 2
      obj.m
      i+=1
    end    
  }
  
  bmr.report('bm_vm2_unif1 '){
    i = 0
    def m a, b
    end

    while i<6000000 # benchmark loop 2
      i+=1
      m 100, 200
    end        
  }  
  
  bmr.report('bm_vm2_zsuper '){
    i = 0

    class C
      def m a
        1
      end
    end

    class CC < C
      def m a
        super
      end
    end

    obj = CC.new

    while i<6000000 # benchmark loop 2
      obj.m 10
      i+=1
    end
  }

end

# Prevent saving this into RubyContext when running under topaz>maglev
Gemstone.abortTransaction rescue nil
