# This is a hack for prototyping
#
# First, we require psych/psych.rb, which is our file and pulls in all of
# the src/experimental/yaml/lib ruby files.  This is the FFI replacement
# code for the Psych's ext code.

require 'psych/psych_ffi'

# After we load our code, stuff the real psych's lib onto the load path and
# then load Psych's ruby code.
$:.unshift "/Users/pmclain/external/psych/lib"
require 'psych'
