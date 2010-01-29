# This is a hack for prototyping
#
# User code will include this file first.  All we do is put the real psych
# on the load path, and then load the real psych.
#
#     yaml/main.rb:             require 'psych'         # this file
#     yaml/lib/psych.rb:        load $P/lib/psych.rb    # Psych
#     $P/lib/psych.rb:          require 'psych/psych'   # Our real entry point
#     yaml/lib/psych/psych.rb   require ffi etc.


# First, we require psych/psych.rb, which is our file and pulls in all of
# the src/experimental/yaml/lib ruby files.  This is the FFI replacement
# code for the Psych's ext code.

# require 'psych/psych_ffi'

# After we load our code, stuff the real psych's lib onto the load path and
# then load Psych's ruby code.
$:.unshift "#{ENV['HOME']}/external/psych/lib"
load "#{ENV['HOME']}/external/psych/lib/psych.rb"
# require 'psych'
