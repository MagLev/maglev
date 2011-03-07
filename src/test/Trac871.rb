# MagLev does not set the __FILE__ name to "xyzzy" for the duration of the
# instance_eval
o = Object.new
o.instance_eval('raise "Fail" unless __FILE__ == "xyzzy"', 'xyzzy', 1)

