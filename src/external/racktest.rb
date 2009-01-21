$:.unshift File.dirname(__FILE__) + '/rack-0.9.0/lib'

require 'webrick'
require 'rack'

class RackTest
  include WEBrick

  def self.test
    app = Proc.new do | env |
      status = 200
      headers = {"Content-Type" => "text/html"}
      body = "<html><h1>Hello From Rack</h1></html>"
      [status, headers, body]
    end
    Rack::Handler::WEBrick.run app, :Port => 3333
  end
end
RackTest.test
