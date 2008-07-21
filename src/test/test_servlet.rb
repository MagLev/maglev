require 'webrick'

class TestServlet < WEBrick::HTTPServlet::AbstractServlet
  def do_GET(req, res) 
        res.body = "hello, world\n"
  end
end

def test_webrick(port)
  s = WEBrick::HTTPServer.new(:Port => port)
  s.mount("/test", TestServlet)
  s.start
end

