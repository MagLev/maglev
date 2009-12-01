#!/usr/bin/env maglev-ruby
require 'webrick'

time_proc = lambda do |req,resp|
  resp['Content-Type'] = "text/html"
  resp.body = %{
    <html>
      <body>
        <p>Hello it is #{Time.now.to_s}</p>
      </body>
    </html>
  }
end

current_time = WEBrick::HTTPServlet::ProcHandler.new(time_proc)

# port = 10700
# s = WEBrick::HTTPServer.new( :BindAddress => '10.80.250.194', :Port => port )
port = 2000
s = WEBrick::HTTPServer.new( :Port => port )
s.mount("/", current_time)
puts "=================================================="
puts "Starting web server:  URL: http://localhost:#{port}/"
puts "=================================================="
puts
s.start


