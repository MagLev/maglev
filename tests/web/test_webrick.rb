require 'net/http'
require 'uri'

url = "http://localhost:2000/"

case (response = Net::HTTP.get_response URI.parse(url))
when Net::HTTPOK
  puts "Ok!"
else
  raise "Fail: #{response}"
end

