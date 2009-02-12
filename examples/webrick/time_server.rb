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

port_num = 10700
s = WEBrick::HTTPServer.new(:Port => port_num )
s.mount("/", current_time)
puts "=================================================="
puts "Starting web server:  URL: http://localhost:#{port_num}/"
puts "=================================================="
puts
s.start


