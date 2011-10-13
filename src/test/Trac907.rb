class Test
  def badCall()
    [ [33,44] , [55,66] ].each do | *args, &block |
      a = args[0]
      b = args[1]
      c = nil # block
      puts "#{a} #{b} #{c}"
      if defined?( Maglev )
        nil.pause
      end
    end
  end

  define_method(:"test1") do |*args, &block|
    x = args.dup 
    x << block.call 
    x
  end

  define_method( :testzero ) do |*args, &block|
    unless args.size == 0 ; raise 'fail';end
    55
  end

  def self.setok(a, b = 999, &cblk)
    yield
  end

  # Another Problem seen in Sinatra with optional args processing
  # cblk was overwritten with 'not_set' value in frame
  def self.setbad(a, b = (not_set = true), &cblk)
    unless not_set._equal?(nil) ; raise 'fail' ; end
    yield
  end
end

y = Test.new.test1( 9, 10, 11 ) { 66 }
unless y = [ 9, 10, 11 , 66 ] ; raise 'fail'; end

$aa = 0
begin
  Test.new.badCall()
rescue ArgumentError
  # expect &blk argument to block not supported in 1.8.7
  $aa = 1
end
unless $aa == 1 ; raise 'fail'; end 

ax = Test.setok( 5, 6) { 99 }
unless ax ==  99 ; raise 'fail'; end
bx = Test.setbad( 7, 8) { 88 }
unless bx ==  88 ; raise 'fail'; end

cx = Test.new.testzero()
unless cx == 55 ; raise 'fail'; end


true
#################### Trac Info
# ID:         907
# Summary:    Problem assigning *args and &block properly
# Changetime: 2011-06-06 23:23:59+00:00
###

#  In Sinatra::Delegator, there is some code generating methods for delegation (base.rb:101). Here, the define_method block takes |*args,&block| as arguments. In the test cases, often, no block is passed, and what happens is that only the first string is passed to as *args, and ''block'' is set to an Array with the remaining args.
#  
#  The following test code demonstrates this:
#  {{{
#  class Test
#    define_method(:"test1") do |*args, &block|
#      puts "*args: #{args.inspect}, &block: #{block.inspect}"
#    end
#  end
#  
#  Test.new.test1 1, "bar", "buzz"
#  => *args: 1, &block: ["bar", "buzz"]
#  }}}
#  