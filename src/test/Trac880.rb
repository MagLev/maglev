# Distilled from Rack::Session::Cookie load_session and commit_session.
#
# MagLev gets:
#
#   $ maglev-ruby pbm.rb
#   error , NoMethodError: undefined method `>' for NilClass,
#                during /Users/pmclain/GemStone/dev/pbm.rb
#   ERROR 2010 , NoMethodError: undefined method `>' for NilClass (MessageNotUnderstood)

x = Object.new
x1 = Marshal.dump(x)
x2 = [x1].pack("m*")

x3 = x2.unpack("m*").first
x4 = Marshal.load(x3)

p x4
