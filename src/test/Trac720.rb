# MagLev does not do all of the ensure blocks
#
# MRI output:
#
#  $ ruby $pbm
#  -- A
#  -- B
#  -- C
#  In Block
#  -- E
#  -- G
#  /Users/pmclain/GemStone/dev/pbm.rb:25: fail (RuntimeError)
#    from /Users/pmclain/GemStone/dev/pbm.rb:10:in `foo'
#    from /Users/pmclain/GemStone/dev/pbm.rb:25
#
# ##############################
# MagLev output:
#
#  $ mruby $pbm
#  -- A
#  -- B
#  -- C
#  In Block
#  #<RuntimeError: fail>
#  /Users/pmclain/GemStone/dev/pbm.rb:25:in `raise'
#  /Users/pmclain/GemStone/dev/pbm.rb:25:in `raise'
#  /Users/pmclain/GemStone/dev/pbm.rb:25
#  /Users/pmclain/GemStone/dev/pbm.rb:10:in `foo'
#  /Users/pmclain/GemStone/dev/pbm.rb:14:in `foo'
#  /Users/pmclain/GemStone/dev/pbm.rb:19:in `foo'
#  /Users/pmclain/GemStone/dev/pbm.rb:25
#  ERROR 2023, Error, 'fail' (RuntimeError)

$aa = []
$bb = []
class C
  def foo
    $aa << 'A'
    aborting = true
    $aa << 'B'
    begin
      $aa << 'C'
      yield
      $aa << 'D'
      aborting = false
      #    rescue Exception => e
    ensure
      $aa << 'E'
    end
    $aa << 'F'
    return :foo
  ensure
    $aa << 'G'
  end

  def foob
    begin 
      $bb << 'A'
      raise
      $bb << 'B'
    ensure
      $bb << 'C'
      #return 9
    end  
    10
  end

  def do_foob
    begin
      $bb << foob
    rescue 
      $bb << 'D'
    end
  end

  def fooc
    begin
      return 7
    ensure
      return 8
    end
  end

  def food
    begin
      $dd << 'A'
    ensure
      $dd << 'B'
      raise 
    end
  end
end

c = C.new

begin
  c.foo { 
    $aa << 'InBlock'
    raise 'anError' 
  }
rescue
  $aa << 'H'
end
puts "$aa = #{$aa.inspect}"
unless $aa == ["A", "B", "C", "InBlock", "E", "G", "H"] ; raise 'failed1'; end

begin
  C.new.do_foob
rescue
  $bb << 'H2'
end
puts "$bb = #{$bb.inspect}"
unless $bb == ["A", "C", "D"] ; raise 'failed2'; end

cx = C.new.fooc
puts "fooc = #{cx}"
unless cx == 8 ; raise 'failed3'; end

$dd = []
begin 
  C.new.food
rescue
  $dd << 'C'
end
puts "$dd = #{$dd.inspect}"
ddx = $dd
unless $dd == ["A", "B", "C"] ; raise 'failed4'; end

true
