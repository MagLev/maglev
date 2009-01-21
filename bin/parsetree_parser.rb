#
#  The http parse server for MagLev.  Start an httpd on port
#  ENV['PARSETREE_PORT'] (or 2001 by default).  The parse server listens
#  for http requests and sends back the s-expressions for the given
#  file/code.
#
#  Sample URLs:
#
#  E.g., either of these URLs will parse $PWD/r.rb:
#
#    http://localhost:2009/parse?dir=$PWD&file=r.rb
#    http://localhost:2009/parse?file=$PWD/r.rb
#
# The raw source is echoed with URLs of this form:
#
#    http://localhost:2009/cat?dir=$PWD&file=./r.rb"
#
# URLs that use the string parameter will parse snippets of ruby code
# (where "..." is replaced with the URI escaped ruby code):
#
#    http://localhost:2001/parse?string=...

$:.unshift File.dirname(__FILE__)  # Ensure we can find maglev_parser.rb

require 'rubygems'
gem 'ParseTree', '2.2.0'

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

# Accepts URLs with query parameters:
#   dir      A directory name
#   file     A filename, or a full path
#   string   Raw ruby code to handle (dir and filename are ignored).
#
# The file to open is calculated by prepending the dir, if any, to the
# filename.  The servlet passes only the filename portion as the filename
# parameter to the parser (for __FILE__ handling).
#
class ParseServlet < HTTPServlet::AbstractServlet
  def do_GET(req, res)
    # The filename is the concatenation of the given part of the file
    # (query["file"]) with the current working directory of the process
    # issuing the http request (query["dir"]).  This allows the parser to
    # expand __FILE__ correctly.
    if(code = req.query['string'])
      dir = nil
      filename = 'eval'
      puts "code: #{code.to_s}"
    else
      filename = req.query['file'].to_s
      given = req.query['given'].to_s
      given ||= filename
      File.open(filename, 'r') do |f|
        code = f.read
      end
    end
    mp = MaglevParser.new
    sexp = mp.parse(code, given, 0).inspect
    # puts sexp
    #  STDOUT.flush
    res.body = sexp
  end
end
s.mount('/parse', ParseServlet)

class CatServlet < HTTPServlet::AbstractServlet
  def do_GET(req, res)
    filename = req.query['file'].to_s
    res.body = File.open(filename, 'r').read
  end
end
s.mount('/cat', CatServlet)

s.start

