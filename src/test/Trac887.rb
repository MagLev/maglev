# coverage for Trac 887

a = Socket.const_get(:Constants)
b = a.name
unless b == 'Socket::Constants' ; raise 'fail'; end

d = File.const_get(:Constants)
e = d.name
unless e = 'File::Constants' ; raise 'fail' ; end
true
#################### Trac Info
# ID:         887
# Summary:    Socket::Constants module name incorrect
# Changetime: 2011-04-06 16:32:24+00:00
###

#  Relatively minor problem, but messes up WebTools display:
#  
#  {{{
#  require 'socket'
#  
#  # MagLev passes this one...
#  raise 'Fail Socket::Constants' unless Socket.const_get(:Constants).name == 'Socket::Constants'
#  
#  # But fails on this:  "Constants" != "File::Constants"
#  raise 'Fail File::Constants'   unless File.const_get(:Constants).name == 'File::Constants'
#  }}}
#  