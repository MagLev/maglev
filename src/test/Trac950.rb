# MagLev was getting an infinite loop here.
# This test case passes if no exception is raised.
class SafeBuffer < String
  def initialize(*args)
    super
  end
end

SafeBuffer.new('x')
true

