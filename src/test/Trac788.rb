# MagLev was giving an error doing a require of an empty file
#
#   <ArgumentError: expected a string>
#   /Users/pmclain/GemStone/dev/pbm.rb:4:in `require'
#   /Users/pmclain/GemStone/dev/pbm.rb:4
#   ERROR 2023, Error, 'expected a string' (ArgumentError)

file = File.dirname(__FILE__) + '/lib/zero_length_ruby_file'
require file
raise "$LOADED_FEATURES does not have blank.rb" unless $LOADED_FEATURES.include? "#{file}.rb"
