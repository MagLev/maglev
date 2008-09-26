require 'rubygems'
require "webrick"
require 'parse_tree'
require 'maglev_parser'

include WEBrick

port = (ENV["PARSETREE_PORT"] || "2001").to_i
    
version = "#{RUBY_VERSION}"
if version != "1.8.6"
    puts "Using incorrect ruby version (#{version})--exiting\n"
    Process.exit!(1)
end

s = HTTPServer.new( :Port => port )
class ParseServlet < HTTPServlet::AbstractServlet
	def do_GET(req, res)
		if(code = req.query["string"])
			filename = "eval"
			puts "code: #{code.to_s}"
		else
			filename = req.query["file"].to_s
			code = ""
			File.open(filename, "r"){|f| code = f.read}
		end
		mp = MaglevParser.new
		sexp = mp.parse(code, filename, 0).inspect
		puts sexp
                STDOUT.flush
		res.body = sexp
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


