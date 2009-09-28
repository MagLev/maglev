# MiniTest suite for the MDB::Server
#
# NOTE: Rack::Test currently does not work in MagLev, so we're using raw
# Rack::MockRequest and Rack::MockResponse.

require 'rubygems'
require 'minitest/spec'
require 'maglev/maglev_json'
require 'rack'

require 'mdb_server'

MiniTest::Unit.autorun

Exception.install_debug_block do |e|
  case e
  when ArgumentError
    nil.pause
  end
end

DB_NAME = 'mdb_server_tests_db'

def get(path)
  @response = @request.get(path)
  handle_response
end

def post(path, data=nil)
  @response = @request.post(path, data.to_json)
  handle_response
end

def handle_response
#   json = case @response
#          when HTTP::Message
#            resp.content
#          else
#            raise "get helper: Unknown type: #{resp.inspect}"
#          end
  JSON.parse(@response.body)[0]
end

describe 'Sinatra mdb_server: Server requests' do
  before do
    @request  = Rack::MockRequest.new(MDB::ServerApp.new)
    @response = nil
  end

  it 'responds to GET "/databases" with an array' do
    r = get '/databases'
    r.class.must_equal Array
  end

  it 'responds to GET /databases/exists/:db appropriately' do
    r = get '/databases'
    r.each do |name|
      r = get "/databases/exists/#{name}"
      r.must_equal true
    end
  end

#   it 'responds to POST "/dbname" by creating a new database' do
#     r = post '/new_db'
#   end

end

