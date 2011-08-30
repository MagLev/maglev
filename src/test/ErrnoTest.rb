require File.expand_path('simple', File.dirname(__FILE__))

# This test currently passes, but only because Errno.constants is stubbed
# to return []...
errnosWithBadSuperclass = Errno.constants.find_all do |n|
  unless n == "MaglevErr" 
    Errno.const_get(n).superclass != SystemCallError 
  end
end
test(errnosWithBadSuperclass.size, 2, "Errnos with bad superclass: #{errnosWithBadSuperclass}")
# Errno::EAGAIN is a SocketError
errnosWithBadSuperclass.each { | n |
  unless n == 'EAGAIN' || n == 'EWOULDBLOCK'
    raise 'failed'
  end
}
test(Errno::EAGAIN.superclass, SocketError, 'Errno::EAGAIN. should be a SocketError')
test(Errno::EWOULDBLOCK.superclass, SocketError, 'Errno::EWOULDBLOCK. should be a SocketError')
test(Errno::EBADF.superclass, SystemCallError, 'Errno::EBADF should be a SystemCallError')


test(Errno::EBADF::Errno, 9, 'Errno::EBADF::Errno != 9')

report
true
