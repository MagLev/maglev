# Test case inspired from core/hash/update_spec.rb
h1 = { :a => 2, :b => -1 }
h2 = { :a => -2, :c => 1 }
h3 = {:c => 1, :b => -1, :a => 3.14}  # guess we're not in Kansas any more...

x = h1.update(h2) { |k,x,y| 3.14 }
raise "Error 1" unless x  == h3
raise "Error 2" unless h1 == h3


# Cut-n-paste from above, except for the send and errors
h1 = { :a => 2, :b => -1 }
h2 = { :a => -2, :c => 1 }
h3 = {:c => 1, :b => -1, :a => 3.14}

x = h1.send(:update, h2) { |k,x,y| 3.14 }
# Maglev triggers both of these errors
raise "Error 3" unless x == h3
raise "Error 4" unless h1 == h3

