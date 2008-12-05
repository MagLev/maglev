require 'webrick'
# include WEBrick

hello_proc = lambda do |req,resp|
  resp['Content-Type'] = "text/html"
  resp.body = %{
    <html>
      <body>
        <p>Hello</p>
      </body>
    </html>
  }
end
bye_proc = lambda do |req,resp|
  resp['Content-Type'] = "text/html"
  resp.body = %{
    <html>
      <body>
        <p>Goodbye</p>
      </body>
    </html>
  }
end

hello = WEBrick::HTTPServlet::ProcHandler.new(hello_proc)
bye   = WEBrick::HTTPServlet::ProcHandler.new(bye_proc)

s = WEBrick::HTTPServer.new(:Port => 2000)
s.mount("/hello", hello)
s.mount("/bye",   bye)
trap("INT") {  s.shutdown }
s.start


