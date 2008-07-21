require "java"
require "webrick"

include_class 'org.jruby.Ruby'
include_class 'java.io.ByteArrayOutputStream'
include_class 'java.io.ObjectOutputStream'

include WEBrick

port = ENV["JRUBY_PORT"].to_i

s = HTTPServer.new( :Port => port )
class ParseServlet < HTTPServlet::AbstractServlet
    def do_GET(req, res)
        if(code = req.query["string"])
            filename = "eval"
            puts code.to_s
        else
            filename = req.query["file"].to_s
            code = ""
            File.open(filename, "r"){|f| code = f.read}
        end
        r = Ruby.getDefaultInstance
        
        ast = r.parse(code.to_s, filename, r.getCurrentContext.getCurrentScope, 0, true)
        f = ByteArrayOutputStream.new
        os = ObjectOutputStream.new(f)
        os.writeObject(ast)   
        res.body = String.from_java_bytes(f.toByteArray)
    end
end
s.mount("/parse", ParseServlet)

class CatServlet < HTTPServlet::AbstractServlet
    def do_GET(req, res)
        filename = req.query['file'].to_s
        res.body = File.open(filename, "r").read
    end
end
s.mount("/cat", CatServlet)

s.start


