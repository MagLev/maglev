require File.expand_path('simple', File.dirname(__FILE__))

# This test currently passes, but only because Errno.constants is stubbed
# to return []...
errnosWithBadSuperclass = Errno.constants.find_all do |n|
  Errno.const_get(n).superclass != SystemCallError
end
test(errnosWithBadSuperclass.size, 0, "Errnos with bad superclass: #{errnosWithBadSuperclass}")

test(Errno::EBADF.superclass, SystemCallError, 'Errno::EBADF should be a SystemCallError')


test(Errno::EBADF::Errno, 9, 'Errno::EBADF::Errno != 9')

report
true
