require 'rack'
require 'webrick'

# For debugging, stop on all exceptions
# Exception.install_debug_block do |e|
#   puts "=== caught #{e.inspect} ================="
#   nil.pause
# end

class RackTest
  include WEBrick

  def self.test
    app = Proc.new do | env |
      status = 200
      headers = {"Content-Type" => "text/html"}
      body = "<html><h1>Hello From Rack</h1><p>Rack.version: #{Rack.version}</p><p>Rack.release: #{Rack.release}</p></html>"
      [status, headers, body]
    end
    Rack::Handler::WEBrick.run app, :Port => 9292
  end
end
RackTest.test
