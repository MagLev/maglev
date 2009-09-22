# Maglev doesn't allow you to rescue on a Class/Module that is not derived
# from Exception.
class I
  module Failure
    def foo
    end
  end
  class IException < ArgumentError
    include Failure
  end
end

def bar
  raise I::IException.new
end

xx = 11
begin
  bar
rescue I::Failure => e
  # Ok...
  xx = 22
rescue Object => e
  raise "Failed to catch the I::Failure..."
rescue Exception => e
  raise "Failed to cat the I::Failure and the Object"
end
unless xx == 22 ; raise 'error' ; end
true
