# https://github.com/MagLev/maglev/issues/127
#
# MRI 1.8.7-p352
#
#   >> 95.to_s(16)
#   => "5f"
#
# MagLev
#
#   >> 95.to_s(16)
#   => "5F"
require File.expand_path('simple', File.dirname(__FILE__))
test 95.to_s(16), "5f", "Fixnum#to_s(16) should return lowercase letters"
