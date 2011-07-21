# maglev-irb is broken
#
#   $ maglev-irb
#   error , no block was passed,
#                during /Users/pmclain/GemStone/snapshots/current/bin/maglev-irb
#   ERROR 2702 , no block was passed (LocalJumpError)
#   topaz 1> 

# this test passes if no exception raised

system "echo exit | #{ENV['MAGLEV_HOME']}/bin/maglev-irb"
raise "Fail: $?: #{$?}" unless $? == 0
