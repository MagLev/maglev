# An example loosely based on RSpec.  Many frameworks have this issue.  The
# framework runs some user level code, and if there is an exception, the
# exception is saved away.  A little while later, the exception backtrace
# is presented to the user, but MagLev prints the wrong stack trace.
#
# == MRI:
#
#   The MRI output from this file:
#     $ ruby $pbm
#     Test tc1: fail
#     /Users/pmclain/GemStone/dev/pbm.rb:41:in `c'
#     /Users/pmclain/GemStone/dev/pbm.rb:37:in `b'
#     /Users/pmclain/GemStone/dev/pbm.rb:33:in `a'
#     /Users/pmclain/GemStone/dev/pbm.rb:46
#     /Users/pmclain/GemStone/dev/pbm.rb:17:in `call'
#     /Users/pmclain/GemStone/dev/pbm.rb:17:in `run'
#     /Users/pmclain/GemStone/dev/pbm.rb:49
#
# == MagLev:
#
#   The MagLev output from this file:
#     $ maglev-ruby $pbm
#     Test tc1: fail
#     /Users/pmclain/GemStone/dev/pbm.rb:39:in `report'
#     /Users/pmclain/GemStone/dev/pbm.rb:62
#
# Notice that the stack trace is *completely* wrong.  It is the stack trace
# as reported from TestCase#report, not from TestCase#run.  This is due to
# the following code in Exception#backtrace:
#
#     def backtrace(limit = 1000)
#       @_st_gsStack || Thread.__backtrace(IncludeSmalltalkFrames, limit)
#     end
#
# If the @_st_gsStack inst var is set (almost never), then the backtrace
# from the thread calling ex.backtrace is printed.  This has NO relation to
# the time the exception was created.
#
# == A "workaround"
#
# One workaround would be for the framework to manually set the backtrace
class TestCase
  def initialize(name, &block)
    @name = name
    @test = block
  end

  def run
    begin
      @test.call
      @status = :pass
    rescue => e
      @status = :fail
      @exception = e
    end
  end

  # An attempt to workaround this in the framework
  def run_enhanced
    begin
      @test.call
      @status = :pass
    rescue => e
      e.set_backtrace(Thread.__backtrace(false, 1_000)) if defined? Maglev
      @status = :fail
      @exception = e
    end
  end

  def report
    puts "\n==========================================="
    puts "Test #{@name}: #{@status}"
    puts @exception.backtrace.join("\n") if @exception
    exa = @exception
  end
end

# Just create an "interesting" backtrace
class C
  def a; b; end
  def b; c; end
  def c 
    raise "failure"
  end
end

tc = TestCase.new("tc1") do
  C.new.a
end
puts "--- Normal"
tc.run
tc.report

puts "--- Workaround "
tc.run_enhanced
tc.report
