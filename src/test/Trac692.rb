# MagLev generates the following error:
#
# error , An attempt was made to change the invariant object 'foo'.,
#           during /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb
# ERROR 2031, An attempt was made to change the invariant object 'foo'. (TypeError)

require 'yaml'
x = { 'foo' => { :bar => :baz }}
p YAML.dump(x)
