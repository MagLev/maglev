require File.expand_path('simple', File.dirname(__FILE__))

require 'socket'

# Just basically test that various methods are present and the format of
# returned info.

tcp_server = TCPServer.open('0.0.0.0', 0)
test(tcp_server.class, TCPServer, 'TCPServer.open with 0 for port')

addr = tcp_server.addr
# Currently, Socket impl is not complete.  Only check fields we know will
# be ok.
test(addr[0], "AF_INET", 'TCPSocket.addr[0]')
test(addr[1].class, Fixnum, 'TCPSocket.addr[1]')  # Randomly assigned port
# test(addr[2], "0.0.0.0", 'TCPSocket.addr[2]')   # we have nil
test(addr[3], "0.0.0.0", 'TCPSocket.addr[3]')

report

