# MagLev does not support multicharacter EOL indicators
#
# $ mruby /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb
#  error , IO#gets, multi-character separator not implemented yet,
#           during /Users/pmclain/GemStone/snapshots/current/src/test/TracXXX.rb
#  ERROR 2023, Error, 'IO#gets, multi-character separator not implemented yet' (ArgumentError)
#

f = File.open(__FILE__)
l = f.gets("open")  # =>  "f = File.open"
p l
