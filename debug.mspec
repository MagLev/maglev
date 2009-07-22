# -*-Ruby-*-
#
# This can be used with the mspec command line to trap any exceptions
# raised during the running of MSpecs.  To use:
#
#    $ spec/mspec/bin/mspec -B debug.mspec \
#        -T -d \
#        -t m spec/rubyspec/language/constants_spec.rb
#
# The "-B debug.mspec" loads this file

# The "-T -d" ensures maglev is started with the -d option (which will drop
# into topaz on a pause or error).
#


# You can customize the block to filter out exceptions you don't want, etc.
if defined? Maglev   # This file is also read by the MRI driver process
  Exception.install_debug_block do |e|
    puts "=== caught #{e.inspect}"
    nil.pause
  end
end
