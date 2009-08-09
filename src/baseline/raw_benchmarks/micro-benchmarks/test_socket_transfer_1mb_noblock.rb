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
    sent = 0
    begin
      result = client.write_nonblock(string[0..(block_size - sent)])
      sent += result
      if(sent < block_size) 
         raise IO::WaitWritable
      end
    rescue IO::WaitWritable, Errno::EINTR
      puts 'retry'
      IO.select(nil, [client])
      retry
    end
  }
  client.close
  receiver.join
end 

[10000, 1000000].map do |n|
  write_meg(n)
end
