# MagLev generates the following error:
#
# error , An attempt was made to change the invariant object 'foo'.,
#           during /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb
# ERROR 2031, An attempt was made to change the invariant object 'foo'. (TypeError)

# The issue is that hash keys are made invariant, and so FFI copies 'foo'
# from object memory to C-memory, libyaml does its thing, but then FFI
# copies from C-memory back into object memory, and tries to overwrite the
# invariant object.  Changing the signature of the FFI call from :string to
# :const_string gets around this, as FFI will not attempt to do the copy
# back into object space.

require 'yaml'
x = { 'foo' => { :bar => :baz }}
p YAML.dump(x)
