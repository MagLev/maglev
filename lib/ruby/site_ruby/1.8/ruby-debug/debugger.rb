# Load the MagLev ruby-debug code, and immediately invoke the debugger.
#
# Typical use case is to require this file at the point you want the
# debugger to stop.  There is no need to require rubygems.:
#
#    # file: foo.rb
#    class C
#      def foo
#        require 'ruby-debug/debugger'   # debugger will stop here when
#                                        # foo is called first time
#      end
#    end
#    C.new.foo                           # trigger debug
#
require 'ruby-debug'
Debugger.start
debugger
