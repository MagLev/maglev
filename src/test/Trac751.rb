# Test case from SCGI + Rack
#
# Maglev was inserting an entry into the hash with a nil key If you remove
# any entry from h1, the bug does not appear, i.e., h1 is a minimal hash to
# show the problem.

h1 = {
  "SCGI"=>"1",
  "HTTP_HOST"=>"localhost:4567",
  "HTTP_CACHE_CONTROL"=>"max-age=0",
  "SERVER_NAME"=>"localhost",
  "SCRIPT_NAME"=>"/",
  "HTTP_ACCEPT_CHARSET"=>"ISO-8859-1,utf-8;q=0.7,*;q=0.3",
  "QUERY_STRING"=>"",
  "SERVER_PROTOCOL"=>"HTTP/1.1",
}


h2 = {}
h2.replace(h1)

puts "h1 has nil key? #{h1.keys.include?(nil)}"
puts "h2 has nil key? #{h2.keys.include?(nil)}"
puts "h2[nil]: #{h2[nil].inspect}"

raise "FAIL with nil key" if h2.keys.include?(nil)
true
