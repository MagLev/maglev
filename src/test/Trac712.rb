# From rails 3
#
# Generates the following error:
#   $ maglev-ruby $pbm
#   -- methodProtection: 2 from nil
#   #<NoMethodError: NoMethodError: private method `_load' for Time>`_load' called
#   /Users/pmclain/GemStone/dev/pbm.rb:16:in `method_missing'
#   /Users/pmclain/GemStone/dev/pbm.rb:16
#   ERROR 2010, NoMethodError: private method `_load' for Time (NoMethodError)

class Time
  class << self
    private
    def quux
    end
  end
end

class Time
  class << self
    def _load(foo)
      987
    end
  end
end

ax = Time._load("")
unless ax == 987 ; raise 'error'; end
true
