require File.dirname(__FILE__) + '/simple'
require 'digest/md5'


module Foo
  # The constants in this module are taken from the ruby specs so we can
  # more easily debug the spec tests.

  Contents = "Ipsum is simply dummy text of the printing and typesetting industry. \nLorem Ipsum has been the industrys standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. \nIt has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. \nIt was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."
  Klass          = ::Digest::MD5
  BlockLength    = 64
  DigestLength   = 16
  BlankDigest    = "\324\035\214\331\217\000\262\004\351\200\t\230\354\370B~"
  Digest         = "\2473\267qw\276\364\343\345\320\304\350\313\314\217n"
  BlankHexdigest = "d41d8cd98f00b204e9800998ecf8427e"
  Hexdigest      = "a733b77177bef4e3e5d0c4e8cbcc8f6e"
end

# Do a digest in one shot
test(Digest::MD5.hexdigest("Some data to crunch on"),
  "971be31d48bf62f5e0256583f257a6d7", 'Test 1')

# incremental digest support
incremental_digest = Digest::MD5.new()
data = [ 'some text',
  'some more text',
  'even more text' ]
data.each do |line|
  incremental_digest << line
end
test(incremental_digest.hexdigest, 'a8db36c1ad1e7577ca2139cc51b53c91', 'incremental')

# d = Digest::MD5.new
# d.update(Foo::Contents)
# dig = d.digest()
# puts "dig.class:   #{dig.class}"
# puts "dig.inspect: #{dig.inspect}"
# puts "dig.to_s:    #{dig.to_s}"
# puts "dig.to_str:  #{dig.to_str}"

report
