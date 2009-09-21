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

begin
  bar
rescue I::Failure => e
  # Ok...
rescue Object => e
  raise "Failed to catch the I::Failure..."
rescue Exception => e
  raise "Failed to cat the I::Failure and the Object"
end

