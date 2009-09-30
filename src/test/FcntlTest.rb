require File.expand_path('simple', File.dirname(__FILE__))

# Call this test before any others, as it tests that Fcntl constants are
# defined before any use of them.

# test(defined? Fcntl::O_APPEND, nil, 'Fcntl constants not defined before require')
# test(defined? Fcntl::O_APPEND, nil, 'Fcntl constants not defined before require (2)')

require 'fcntl'

test(defined? Fcntl::O_APPEND, "constant", 'Fcntl constants defined at boot')
test(defined? Fcntl::F_SETFD, "constant", 'Fcntl::F_SETFD')
test(defined? Fcntl::FD_CLOEXEC, "constant", 'Fcntl::FD_CLOEXEC')
test(defined? Fcntl::F_SETFL, "constant", 'Fcntl::F_SETFL')

report

