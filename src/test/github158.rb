# https://github.com/MagLev/maglev/issues/158
#
# popen does not set $?

f = IO.popen("uname") { |pipe| pipe.read }
raise "$? is nil." unless $?
