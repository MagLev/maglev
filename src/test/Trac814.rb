# This code raises the following exception:
#
#  ERROR 2010, No method was found for the selector #'resolveConstant:'
#        when sent to nil with arguments contained in anArray(
#        #'IncludeSmalltalkFrames'). (NoMethodError)
#
klass = Errno::EAGAIN
klass.autoload?(:IncludeSmalltalkFrames)
