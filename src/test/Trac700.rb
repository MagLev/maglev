# Distilled from ActiveSupport time zone.
#
# The real file instantiates a whole bunch of time zones
#
# MagLev fails with:
#
# <RuntimeError: An attempt was made to create an instance of the
#  modifiable class TimeZone .  Send "immediateInvariant" to the class to
#  terminate class modification and allow instance creation.>
#
# /Users/pmclain/GemStone/dev/pbm.rb:2:in `new'
# /Users/pmclain/GemStone/dev/pbm.rb:2:in `__compileClass'
# /Users/pmclain/GemStone/dev/pbm.rb:1
# ERROR 2009, An attempt was made to create an instance of the modifiable class TimeZone .  Send "immediateInvariant" to the class to terminate class modification and allow instance creation. (RuntimeError)

class TimeZone
  unless const_defined?(:ZONES)
    ZONES = new
  end
end
