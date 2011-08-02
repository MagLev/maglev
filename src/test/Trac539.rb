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
      # inner block should be a copying block containing copy of   handler
      parser.on { |value| handler.call(value) }
    end

yy =  parser.parse!
unless yy == [19, 29, 39, 49] ; raise 'ERROR'; end
true

class C934
  # code from Trac 934
  def initialize
    @blks = [ ]
  end
  def save_call(&block) 
    @blks << block
  end
  def test
    trc = String.new
    sx = nil
    # exit_code should be in the VC, not passed via instvar of copying blocks
    save_call {
      trc << "aa"
      exit_code = nil
      save_call {
        trc << "bb"
        sx = exit_code
      }
      exit_code = 123
    }
    j = 0
    while j < @blks.size
      @blks[j].call
      j += 1
    end
    unless trc == 'aabb' ; raise 'fail'; end
    unless sx == 123 ; raise 'fail'; end
  end
end
C934.new.test
