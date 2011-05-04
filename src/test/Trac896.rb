# Ensure that MagLev respects RUBYOPT
#
# Add /tmp to $LOAD_PATH via RUBYOPT and then test to be sure it is there.

result = `RUBYOPT="-I/tmp" maglev-ruby -e 'p $LOAD_PATH'`
raise "Fail" unless result =~ %r{.*/tmp.*}
