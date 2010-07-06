require File.dirname(__FILE__) + '/simple'

# do not require other files, 'digest/md5', since we need to test that we
# can access MD5 through Digest's factory methods.
require 'digest'

module Foo
  # The constants in this module are taken from the ruby specs so we can
  # more easily debug the spec tests.

  Contents = "Ipsum is simply dummy text of the printing and typesetting industry. \nLorem Ipsum has been the industrys standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. \nIt has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. \nIt was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

  ContentsAsHexdigest = "497073756d2069732073696d706c792064756d6d792074657874206f6620746865207072696e74696e6720616e64207479706573657474696e6720696e6475737472792e200a4c6f72656d20497073756d20686173206265656e2074686520696e64757374727973207374616e646172642064756d6d79207465787420657665722073696e6365207468652031353030732c207768656e20616e20756e6b6e6f776e207072696e74657220746f6f6b20612067616c6c6579206f66207479706520616e6420736372616d626c656420697420746f206d616b65206120747970652073706563696d656e20626f6f6b2e200a497420686173207375727669766564206e6f74206f6e6c7920666976652063656e7475726965732c2062757420616c736f20746865206c65617020696e746f20656c656374726f6e6963207479706573657474696e672c2072656d61696e696e6720657373656e7469616c6c7920756e6368616e6765642e200a49742077617320706f70756c61726973656420696e207468652031393630732077697468207468652072656c65617365206f66204c657472617365742073686565747320636f6e7461696e696e67204c6f72656d20497073756d2070617373616765732c20616e64206d6f726520726563656e746c792077697468206465736b746f70207075626c697368696e6720736f667477617265206c696b6520416c64757320506167654d616b657220696e636c7564696e672076657273696f6e73206f66204c6f72656d20497073756d2e"

  Klass          = ::Digest::MD5
  BlockLength    = 64
  DigestLength   = 16
  BlankDigest    = "\324\035\214\331\217\000\262\004\351\200\t\230\354\370B~"
  Digest         = "\2473\267qw\276\364\343\345\320\304\350\313\314\217n"
  BlankHexdigest = "d41d8cd98f00b204e9800998ecf8427e"
  Hexdigest      = "a733b77177bef4e3e5d0c4e8cbcc8f6e"
end

# Test Digest
test(Digest.hexencode(Foo::Contents), Foo::ContentsAsHexdigest, 'Digest.hexdigest()')

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

# Test a SHA1 digest
# plain = "some plain text"
# expected = "\0258\301G{\227GI\377\373\370\037\235\016%\2430e@v"
# test(Digest::SHA1.digest(plain), expected, 'SHA1 test')

# Test incremental usage of SHA1
#sha1 = Digest::SHA1
report
