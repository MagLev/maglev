# This causes a local jump error.
#
# Protect against environment that don't have tilt installed
begin
  require 'rubygems'
  require 'erb'
  require 'tilt'

  TDIR = File.join(File.dirname(__FILE__), 'test_data')
  X      = File.join(TDIR, 'x.erb')
  LAYOUT = File.join(TDIR, 'layout.erb')
  class C
    # This include turns on the class << self in Tilt
    include Tilt::CompileSite

    def render(x, &block)
      template = Tilt.new(X)
      layout   = Tilt.new(LAYOUT)

      # render template
      output = template.render(self, { }, &block)

      # render layout
      output = layout.render { output }
      p output
    end
  end

  C.new.render(10) { "block to render" }
  true
rescue LoadError => e
  puts "------- SKIPPED #{$0}  #{e}"
  false
end
#################### Trac Info
# ID:         808
# Summary:    Installing tilt gem causes errors
# Changetime: 2010-11-01 19:36:33+00:00
###

#  If I install the tilt gem, sinatra 1.0 examples fail with a  "LocalJumpError - no block was passed"
#  
#  Note: "WARN  TCPServer Error: SocketError_unknown getHostAddressByName failed" is expected on my Mac.
#  It doesn't cause problems running the examples.
#  Tilt seems to be the cause of the localjump error -- if i "maglev-gem uninstall tilt", the error goes away.
#  
#  {{{
#  maglev-gem install sinatra tilt
#  cd $MAGLEV_HOME/examples/sinatra/simple_blog/
#  rake blog
#  (in /Users/monty/MagLev/MagLev-24518.Darwin-i386/examples/sinatra/simple_blog)
#   maglev-ruby -Ilib lib/commit_code.rb  
#  == blog.rb is already committed....skipping
#   $MAGLEV_HOME/bin/rackup config.ru 
#  ----- MAIN OBJECT: 
#  [2010-10-21 08:22:34] INFO  WEBrick 1.3.1
#  [2010-10-21 08:22:34] INFO  ruby 1.8.7 (2010-10-20) [x86_64-darwin]
#  [2010-10-21 08:22:34] WARN  TCPServer Error: SocketError_unknown getHostAddressByName failed
#  [2010-10-21 08:22:34] WARN  TCPServer Error: SocketError_unknown getHostAddressByName failed
#  [2010-10-21 08:22:34] INFO  WEBrick::HTTPServer#start: pid=20755 port=4444
#  127.0.0.1 - - [21/Oct/2010 08:22:39] "GET / HTTP/1.1" 302 - 0.0119
#  LocalJumpError - no block was passed:
#  
#  }}}
#  