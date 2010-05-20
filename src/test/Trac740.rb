require 'socket'
p Socket.getaddrinfo("localhost", 4567, 0, Socket::SOCK_DGRAM)
p Socket.getaddrinfo("localhost", 4567, "AF_INET")
p Socket.getaddrinfo("localhost", 4567)
true
