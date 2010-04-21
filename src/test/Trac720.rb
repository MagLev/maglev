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

class C
  def foo
    puts "-- A"
    aborting = true
    puts "-- B"
    begin
      puts "-- C"
      yield
      puts "-- D"
      aborting = false
      #    rescue Exception => e
    ensure
      puts "-- E"
    end
    puts "-- F"
    return :foo
  ensure
    puts "-- G"
  end
end

c = C.new

#begin
  c.foo { puts "In Block"; raise 'fail' }
#rescue
#  puts "In top level"
#end

