h = { }

# Used to raise ERROR 2010, Undefined method `to_hash' for nil
raise 'fail 1' unless  (h == nil) == false

raise 'fail 2' unless h != nil
#################### Trac Info
# ID:         504
# Summary:    Hash#== not correct
# Changetime: 2009-04-26 20:32:39+00:00
###

#  The implementation of == for RubyHash (alias as Hash) is not correct.  It assumes a method to_hash that is not required of other objects being compared to hashes.  Please use the Ruby definition for equality.