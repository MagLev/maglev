# MagLev was returning:
#
# error , SocketError_unknown  port name not found,
#           during /Users/pmclain/GemStone/checkouts/git/src/test/TracXXX.rb
# ERROR 2023, Error, 'SocketError_unknown  port name not found' (SocketError)

require 'socket'

address = "0.0.0.0"   # Equivalent to INADDR_ANY
port = "4567"

res = Socket::getaddrinfo(address, port,
                          Socket::AF_UNSPEC,   # address family
                          Socket::SOCK_STREAM, # socket type
                          0,                   # protocol
                          Socket::AI_PASSIVE)  # flag
puts res.inspect

unless res.length == 1 ; raise 'err;'; end
elem = res[0]
exp = [ "AF_INET", 4567, "0.0.0.0", "0.0.0.0", 2 ]  # elements that are same on Solaris and Linux
act = [ elem[0], elem[1], elem[2], elem[3], elem[4] ]
unless exp == act
  raise "error: expected #{exp.inspect}  actual: #{act.inspect}"
end

res = Socket::getaddrinfo("moro", "http")
puts res.inspect
unless res.length >= 1; raise 'err;'; end # 1 on Solaris, 2 on Linux

elem = res[0]
expA = ["AF_INET", 80, "moro.gemstone.com", "10.80.250.115", 2 ]
expB = ["AF_INET", 80, "moro",              "10.80.250.115", 2 ]
act = [ elem[0], elem[1], elem[2],         elem[3], elem[4] ]
unless act == expA || act == expB
  raise "error: expected #{expA.inspect}  actual: #{act.inspect}"
end
true
