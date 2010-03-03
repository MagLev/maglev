# From Rails3 ActiveSupport
#

module ClassMethods
  def __create_keyed_callback(name, kind, object, &blk) #:nodoc:
    @_keyed_callbacks[name] ||= begin
      str = send("_#{kind}_callbacks").compile(name, object)
    end
  end
end

class C
  def initialize
    @arr = [ nil ]
  end
  def setvar(idx)
    @arr[idx] ||= begin
      [1,2,3,4,5,6,7].size
    end
  end
  def get
    @arr[0]
  end
  def many
    str = 'abc'
    @arr[0] += begin
      str << 'abcd'
      str << 'defg'
      str.size
    end
  end
end

o = C.new
o.setvar(0)
unless o.get == 7; raise 'error'; end
o.many
unless o.get == 18; raise 'error'; end
true
