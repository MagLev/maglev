require 'socket'

def write_meg(block_size)
  a = TCPServer.new ''
  port = a.addr[1]
  client = TCPSocket.new nil, port
  server = a.accept
  receiver = Thread.new {
    loop { 
      got = server.recv 10 
      break if got.length == 0 # closed socket
    }  
  }
  string = 'a'*block_size
  (1000000/block_size).times {
    client.write string
  }
  client.close
  receiver.join
end 

[10000, 1000000].map do |n|
  puts "Testing #{n}"
  write_meg(n)
end
