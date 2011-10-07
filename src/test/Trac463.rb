# trac 463  coverage for  next  and   break

unless defined?(Maglev)
  class NilClass
    def pause
    end
  end
end

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
      puts "n = #{n}" if $debugx
      n += 1
      if n >= 10
        nil.pause if $debugx
        break 95
      end
      begin
        if n >= 10
          nil.pause if $debugx
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

  # more uses of break from the ruby specs
  def testArrayNew
     a = Array.new(3) do |i|
       if i == 2
         nil.pause if $debugx
         break 98 
       end
       i.to_s
     end
     unless a == 98; raise 'fail'; end
  end

  def testClsNew
     klass = Class.new do
       def initialize
	 yield
       end
     end
     x = klass.new { break 42 }
     unless x == 42 ; raise 'fail'; end
  end
end

class BreakTest
  # case #1: yield
  def self.fail(msg)
    raise "error #{msg}"
  end
  def self.meth_with_yield(&b)
    yield
    fail("break returned from yield to wrong place")
  end
  def self.invoking_method(&b)
    meth_with_yield(&b)
    fail("break returned from 'meth_with_yield' method to wrong place")
  end

  # case #2: block.call
  def self.meth_with_block_call(&b)
    b.call
    fail("break returned from b.call to wrong place")
  end
  def self.invoking_method2(&b)
    meth_with_block_call(&b)
    fail("break returned from 'meth_with_block_call' method to wrong place")
  end

  def self.test
    # this calls a method that calls another method that yields to the block
    ax = self.invoking_method do
      break 46
      fail("break didn't, well, break")
    end
    unless ax == 46 ; raise "#{ax} not equal 46"; end
    puts "okA"

    # this calls a method that calls another method that calls the block
    bx = self.invoking_method2 do
      break 47
      fail("break didn't, well, break")
    end
    unless bx == 47 ; raise "#{bx} not equal 46"; end
    puts "okB"

    res = self.invoking_method do
      break 35
      fail("break didn't, well, break")
    end
    unless res == 35 ; raise "#{res} not equal 35"; end
    puts "okC"

    res = self.invoking_method2 do
      break 36
      fail("break didn't, well, break")
    end
    unless res == 36 ; raise "#{res} not equal 36"; end
    puts "BreakTest.test ok"
  end
end

class TUnit
  def _wrap_assertion(&blk)
    blk.call
  end
  def assert_block(msg, &blk)
    blk.call(msg)
  end
  def assert_raise(*args, &blk)
    _wrap_assertion do
      actual_exception = nil
      full_message = 'abc'
      rx = assert_block(full_message) do
	begin
	  blk.call
	rescue TypeError => actual_exception
	  break 776
	end
	false
      end
      unless rx == 776 ; raise 'fail'; end
      unless actual_exception.class == TypeError; raise 'fail'; end
      455
    end
  end

  def test
    x = assert_raise() { raise TypeError }
    unless x == 455 ; raise 'fail'; end
    puts "aTUnit.test ok"
  end
end

$debugx = false

o = Trac463.new
o.testArrayNew
o.testClsNew
TUnit.new.test

o.testOne
o.testTwo
o.testThree
o.testTwoU
o.testThreeU

$debugx = true

BreakTest.test

class Brk2
  def self.break_test()
    yield 1
    yield 2
    yield 3
  end
  ax = break_test {|i| break i if i == 2 }
  unless ax == 2 ; raise "#{ax} not equal 2"; end
  i = 0 
  bx = break_test {|i| break i if i == 1 }
  unless i == 0 ; raise "#{i} not equal 1"; end # 1.9 behavior
  puts "Brk2 done"
end

class Brk3
  def self.mx
    break
  end
  ax = 0
  begin
    (lambda { mx }).call
  rescue LocalJumpError
    ax = 23
  end
  unless ax == 23 ; raise 'failed';end
  puts "Brk3 done"
end


puts "OK"
true
#################### Trac Info
# ID:         463
# Summary:    "next" and "rescue" do not play well together (core dump)
# Changetime: 2009-09-21 20:57:39+00:00
###

#  If you try to skip to the next iteration of a loop with {{{next}}} from inside a {{{begin}}}/{{{rescue}}}/{{{end}}} construct, things do not go well.  For example:
#  
#  {{{
#  $q = ['boo']
#  
#  while not $q.empty?
#      begin
#          next if $q.shift
#        rescue ZeroDivisionError => err
#          p "This never happens"
#        end
#     end
#  }}}
#  
#  {{{
#  (markus@glass) ~/maglev/MagLev-21530.Linux/irb2> maglev-ruby next_bug.rb
#  ERROR 2079, GemStone Smalltalk execution could not return from the current Activation. 'hit reenter marker in Bc_RUBY_NEXT' Home 
#  context of block to return from may no longer be active.GemStone Smalltalk execution could not return from the current Activation.
#  'hit reenter marker in Bc_RUBY_NEXT' Home context of block to return from may no longer be active.
#  }}}
#  
#  If the example is simplified further (to an infinite loop as opposed to one that exits after the first iteration) the failure is even more dramatic:
#  
#  {{{
#  while true
#      begin
#          next
#        rescue ZeroDivisionError => err
#          p "This never happens"
#        end
#     end
#  }}}
#  
#  
#  {{{
#  Gemstone Signal Handler: Signal 11,  SIGSEGV Received
#  HostFaultHandler: signal = 11
#      info->si_signo = 11 = 0xb
#      info->si_code =  128 = 0x80
#      info->si_errno = 0 = 0x0
#      info->si_addr = (nil)
#    Registers saved from frame receiving the signal:
#     rip 0x7f1bcdd2430f rsp 0x7fffd744edf0 rbp 0x7f1bce064f50
#     rax 0x7f1bb06ca348 rbx 0x20 rcx 0x3c1a0000011c82 rdx 0x7f1bc22b2ff8
#     r8 0x7f1bafedcc28 r9 0x14 r10 0x7f1bc2288a78 r11 0x246
#     r12 0x7f1bce064f50 r13 0x1 r14 0x7f1bc22b30b8 r15 0x7f1bafedcc58
#     rdi 0x7f1bce064f50 rsi 0x7f1bce065ad8 efl 0x10206 csgsfs_pad 0xe033
#     err 0x0 trapno 0xd oldmask 0x0 cr2 0x0
#  
#  Begin attempt to print C-level stack at: Sun Apr 19 00:45:36 UTC 2009
#  
#  
#  End of C-level stack:
#  
#  
#  
#   _____________________________________________________________________________
#  | Possible Internal Error: HostCoreDump invoked at 04/19/09 00:45:36.729 UTC
#   in process 13636
#  
#   __________________________________
#  HostCoredump: Waiting 60 seconds for C Debugger to attach, process 13636
#  }}}
#  