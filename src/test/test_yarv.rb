### Ruby benchmark file for yarv_small
require 'benchmark'
Benchmark.bm(19) do |bmr|

bmr.report("bm_app_answer          "){
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

answer = the_answer_to_life_the_universe_and_everything
}

bmr.report("bm_app_factorial       "){
def fact(n)
  if(n > 1)
    n * fact(n-1)
  else
    1
  end
end

fact(3000)
fact(3000)
fact(3000)
fact(3000)
fact(3000)

}

bmr.report("bm_app_fib             "){
def fib n
  if n < 3
    1
  else
    fib(n-1) + fib(n-2)
  end
end

fib(34)

}

bmr.report("bm_app_strconcat       "){
i=0
while i<500000
  "#{1+1} #{1+1} #{1+1}"
  i+=1
end
}

bmr.report("bm_app_tak             "){

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

bmr.report("bm_app_tarai           "){
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

bmr.report("bm_loop_times          "){
30000000.times{|e|}
}

bmr.report("bm_loop_whileloop      "){
i = 0
while i<30000000 # benchmark loop 1
  i+=1
end
}

bmr.report("bm_loop_whileloop2     "){
i=0
while i<60000000 # benchmark loop 2
  i+=1
end

}

bmr.report("bm_so_ackermann        "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: ackermann-ruby.code,v 1.4 2004/11/13 07:40:41 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

def ack(m, n)
    if m == 0 then
        n + 1
    elsif n == 0 then
        ack(m - 1, 1)
    else
        ack(m - 1, ack(m, n - 1))
    end
end

NUM = 7
ack(3, NUM)


}

bmr.report("bm_so_array            "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: ary-ruby.code,v 1.4 2004/11/13 07:41:27 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Paul Brannan and Mark Hubbart

n = 9000 # Integer(ARGV.shift || 1)

x = Array.new(n)
y = Array.new(n, 0)

n.times{|bi|
  x[bi] = bi + 1
}

(0 .. 999).each do |e|
  (n-1).step(0,-1) do |bi|
    y[bi] += x.at(bi)
  end
end
# puts "#{y.first} #{y.last}"


}

bmr.report("bm_so_concatenate      "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: strcat-ruby.code,v 1.4 2004/11/13 07:43:28 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# based on code from Aristarkh A Zagorodnikov and Dat Nguyen

STUFF = "hello\n"
i=0
while i<10
  i+=1
  hello = ''
  400000.times do |e|
    hello << STUFF
  end
end
# puts hello.length


}

bmr.report("bm_so_exception        "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: except-ruby.code,v 1.4 2004/11/13 07:41:33 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

$HI = 0
$LO = 0
NUM2 = 250000 # Integer(ARGV[0] || 1)


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
max = NUM2+1
while i < max
  i+=1
  some_function(i+1)
end
}

bmr.report("bm_so_matrix           "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: matrix-ruby.code,v 1.4 2004/11/13 07:42:14 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/

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

bmr.report("bm_so_nested_loop      "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: nestedloop-ruby.code,v 1.4 2004/11/13 07:42:22 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# from Avi Bryant

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
# puts x


}

bmr.report("bm_so_object           "){
#!/usr/bin/ruby
# -*- mode: ruby -*-
# $Id: objinst-ruby.code,v 1.4 2004/11/13 07:42:25 bfulgham Exp $
# http://www.bagley.org/~doug/shootout/
# with help from Aristarkh Zagorodnikov

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

bmr.report("bm_so_random           "){
# from http://www.bagley.org/~doug/shootout/bench/random/random.ruby

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
# "%.9f" % gen_random(100.0)
}

bmr.report("bm_vm1_block           "){
def m
  yield
end

i=0
while i<30000000 # while loop 1
  i+=1
  m{
  }
end}

bmr.report("bm_vm1_const           "){
Const = 1

i = 0
while i<30000000 # while loop 1
  i+= 1
  j = Const
  k = Const
end
}

bmr.report("bm_vm1_ensure          "){
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

bmr.report("bm_vm1_length          "){
a = 'abc'
b = [1, 2, 3]
i=0
while i<30000000 # while loop 1
  i+=1
  a.length
  b.length
end

}

bmr.report("bm_vm1_rescue          "){
i=0
while i<30000000 # while loop 1
  i+=1
  begin
  rescue
  end
end
}

bmr.report("bm_vm1_simplereturn    "){
def m
  return 1
end
i=0
while i<30000000 # while loop 1
  i+=1
  m
end

}

bmr.report("bm_vm1_swap            "){
a = 1
b = 2
i=0
while i<30000000 # while loop 1
  i+=1
  a, b = b, a
end

}

bmr.report("bm_vm2_array           "){
i=0
while i<6000000 # benchmark loop 2
  i+=1
  a = [1,2,3,4,5,6,7,8,9,10]
end
}

bmr.report("bm_vm2_method          "){
def m
  nil
end

i=0
while i<6000000 # benchmark loop 2
  i+=1
  m; m; m; m; m; m; m; m;
end
}

bmr.report("bm_vm2_poly_method     "){
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

bmr.report("bm_vm2_poly_method_ov  "){
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

bmr.report("bm_vm2_proc            "){
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

bmr.report("bm_vm2_regexp          "){
i=0
str = 'xxxhogexxx'
while i<6000000 # benchmark loop 2
  /hoge/ =~ str
  i+=1
end
}

bmr.report("bm_vm2_send            "){
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

bmr.report("bm_vm2_super           "){

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

bmr.report("bm_vm2_unif1           "){
i = 0
def m a, b
end

while i<6000000 # benchmark loop 2
  i+=1
  m 100, 200
end
}

bmr.report("bm_vm2_zsuper          "){
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
