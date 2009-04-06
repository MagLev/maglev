require File.dirname(__FILE__) + '/simple'
require 'digest/md5'

# Do a digest in one shot
#digest = Digest::MD5.hexdigest(File.read("a_file"))
digest = Digest::MD5.hexdigest("Some data to crunch on")
p digest
test(digest, "971be31d48bf62f5e0256583f257a6d7", 'Test 1')

# # incremental digest support
# incremental_digest = Digest::MD5.new()
# File.open("some_file") do |f|
#   f.each_line do |line|
#     incremental_digest << line
#   end
# end
# puts incremental_digest.hexdigest

report
