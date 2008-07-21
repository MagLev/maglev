require 'webrick'

class HelloServlet < WEBrick::HTTPServlet::AbstractServlet
    def do_GET(req, res) 
        res.body = "<h1>Hello, RailsConf</h1><font size='24'><pre>#{$hat.inspect.gsub(/</, "&lt;")}</pre></font>"
        res["Content-Type"] = "text/html"
    end
end

def test_webrick
s = WEBrick::HTTPServer.new(:Port => 9001)
s.mount("/hello", HelloServlet)
s.start
end