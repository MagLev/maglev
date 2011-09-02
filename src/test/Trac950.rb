# MagLev was getting an infinite loop here.
# This test case passes if no exception is raised.
class SafeBuffer < String
  def initialize(*args)
    super
  end
end

sx = SafeBuffer.new('abc')
unless sx[0,3] == 'abc' ; raise 'fail'; end
true

