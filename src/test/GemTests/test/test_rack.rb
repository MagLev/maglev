require 'rubygems'
require 'bundler/setup'

require 'minitest/unit'
require 'rack/test'

MiniTest::Unit.autorun

class RackTest < MiniTest::Unit::TestCase
  include Rack::Test::Methods

  def app
    Proc.new do | env |
      status = 200
      headers = {"Content-Type" => "text/html"}
      body = "<html><h1>Hello From Rack</h1><p>Rack.version: #{Rack.version}</p><p>Rack.release: #{Rack.release}</p></html>"
      [status, headers, body]
    end
  end

  def test_it_works
    get '/'
    assert last_response.ok?, "response not ok"
    assert last_response.body =~ /Hello From Rack/
  end
end
