# Distilled from optparse.rb
#
# The problem is that in the option_list.each loop, each
# same underlying block, so that all of the option handlers get the last block
# rather than their own Proc
class Parser
  def initialize
    @blocks = Array.new
  end

  def on(&block)
    @blocks << block
  end

  def parse!
    res = []
    ary = @blocks
    n = 0
    lim = ary.size
    while n < lim
      res << ary[n].call( 9 )
      n += 1
    end
    res
  end
end

class C
  def test
    option_list = [
      Proc.new { |a| 10 + a } ,
      Proc.new { |a| 20 + a } ,
      Proc.new { |a| 30 + a } ,
      Proc.new { |a| 40 + a }
    ]
    parser = Parser.new

    option_list.each do |handler|
      parser.on { |value| handler.call(value) }
    end
    
    parser.parse!
  end
end


# test case for a method not creating a binding
xx = C.new.test
unless xx == [19, 29, 39, 49] ; raise 'ERROR'; end

# now execute again at top level (which has an implicit binding)
    option_list = [
      Proc.new { |a| 10 + a } ,
      Proc.new { |a| 20 + a } ,
      Proc.new { |a| 30 + a } ,
      Proc.new { |a| 40 + a }
    ]
    parser = Parser.new
    option_list.each do |handler|
      parser.on { |value| handler.call(value) }
    end

yy =  parser.parse!
unless yy == [19, 29, 39, 49] ; raise 'ERROR'; end
true

