require 'socket'

def write_meg(block_size, test_size)
  puts "block_size #{block_size}  test_size #{test_size}"
  a = TCPServer.new('')
  port = a.addr[1]
  client = TCPSocket.new(nil, port)
  server = a.accept
  receiver = Thread.new {
    rcount = 0
    loop { 
      got = server.recv(100)
      if got.equal?(nil)  # EOF on socket  # Maglev
        puts "EOF, total received = #{rcount}"
        unless rcount >= test_size 
          raise 'lost data'
        end
	break  
      end
      rcount += got.length
    }  
  }
  string = 'a'*block_size
  wcount = 0
  lastprt = 0
  while wcount < test_size
    numwrote = client.write(string)
    wcount += numwrote
  end
  puts "wrote = #{wcount} , waiting for receiver to finish"
  client.close
  receiver.join
end 

write_meg(100,   10000)
write_meg(1000,  100000)
write_meg(10000,   1000000)
write_meg(1000000, 1000000)
true
