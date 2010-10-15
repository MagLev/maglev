require 'socket'

STDIN.reopen(TCPServer.new(5555))

puts "OK"
