require 'socket'

server = TCPServer.new 2000 # Server bind to port 2000
output = []

def read_socket_message(socket, chunk_size = 2)
  buffer = ""
  while ! buffer.end_with?("\n")
    IO.select([socket])
    buffer << socket.read_nonblock(chunk_size)
  end
  buffer
end

server_thread = Thread.new do
  loop do
    client = server.accept    # Wait for a client to connect
    message = read_socket_message(client)
    output << "Server :: client says: #{message}"
    if message == "die!\n"
      output << "Server :: farewell cruel world!"
      client.close
      break
    else
      client.puts "Hello!"
      client.close
    end
  end
end

client_thread = Thread.new do
  socket = TCPSocket.new('127.0.0.1', 2000)
  socket.puts("ping")
  response = read_socket_message(socket)
  output << "Client :: server says: #{response}"
  socket = TCPSocket.new('127.0.0.1', 2000)
  socket.puts("die!")
end

client_thread.join
server_thread.join

expected = ["Server :: client says: ping\n","Client :: server says: Hello!\n","Server :: client says: die!\n","Server :: farewell cruel world!"]

raise "TCPSocket#read_nonblock didn't work" unless output == expected
