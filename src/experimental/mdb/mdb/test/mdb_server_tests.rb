# MiniTest suite for the MDB::Server
require 'rubygems'
require 'minitest/spec'
require 'httpclient'
require 'json'

MiniTest::Unit.autorun

SERVER = 'http://localhost:4567'
DB_NAME = 'mdb_server_tests_db'

def get(path)
  handle_response HTTPClient.get(SERVER + path)
end

def post(path, data=nil)
  handle_response HTTPClient.post(SERVER + path, data.to_json)
end

def handle_response(resp)
  json = case resp
         when HTTP::Message
           resp.content
         else
           raise "get helper: Unknown type: #{resp.inspect}"
         end
  JSON.parse(json)[0]
end

describe 'Sinatra mdb_server: Server requests' do
  before do
  end

  it 'responds to GET "/databases" with an array' do
    r = get '/databases'
    r.class.must_equal Array
  end

# TODO: Blocked on Trac 616
#   it 'responds to GET /databases/exists/:db appropriately' do
#     r = get '/databases'
#     r.each do |name|
#       r = get "/databases/exists/#{name}"
#       r.must_equal true
#     end
#   end

#   it 'responds to POST "/dbname" by creating a new database' do
#     r = post '/new_db'
#   end

end

