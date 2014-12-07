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

url = "www.gemtalksystems.com"
res = Socket::getaddrinfo(url, "http")
puts res.inspect
unless res.length >= 1; raise 'err;'; end # 1 on Solaris, 2 on Linux

elem = res[0]
unless %w[ AF_INET AF_INET6 ].include?(elem[0]) && elem[1] == 80
  raise "error: expected 'AF_INET' and port 80: actual #{elem[0]} #{elem[1]}"
end

# The actual value for this changes depending on whether you are inside or
# outside of the firewall, so check for either case.
unless elem[2] =~ /gemtalksystems.com/ ||
    elem[2] =~ /akamaitechnologies/ ||
    elem[2] =~ /#{(resolved = `resolveip -s #{url}`.chomp)}/
  raise "error: gemtalksystems.com, akamaitechnologies, or #{resolved}:  actual #{elem[2]}"
end

unless elem[3] =~ /\d{2,3}\.\d{1,3}\.\d{1,3}/
  raise "error: expected inet number:  actual #{elem[3]}"
end
true
