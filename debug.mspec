# -*-Ruby-*-
#
# This can be used with the mspec command line to trap any exceptions
# raised during the running of MSpecs.  To use:
#
#    $ spec/mspec/bin/mspec -B debug.mspec -t m spec/rubyspec/language/constants_spec.rb
#
# You can customize the block to filter out exceptions you don't want, etc.
if defined? Maglev
  Exception.install_debug_block do |e|
    puts "=== caught #{e.inspect}"
    nil.pause
  end
end
