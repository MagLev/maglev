# MiniTest suite for the MDB::Server
#
# NOTE: Rack::Test currently does not work in MagLev, so we're using raw
# Rack::MockRequest and Rack::MockResponse.

require 'rubygems'
require 'minitest/spec'
require 'maglev/maglev_json'
require 'rack'

require 'mdb_server'
#require 'app_model.rb'  # For the view class # TODO: just move in here?

class FixtureView
  def view_53
    53
  end
end

MiniTest::Unit.autorun

Exception.install_debug_block do |e|
  case e
  when ArgumentError
    nil.pause
  when NoMethodError
    nil.pause if e.message =~ /merge/
  end
end

def get(path)
  @response = @request.get(path)
  handle_response
end

def post(path, data=nil)
  @response = @request.post(path, data)
  handle_response
end

def handle_response
  JSON.parse(@response.body)[0]
end


DB_NAME   = 'mdb_server_tests_db'  # Will be created fresh in before()
DB_NAME_2 = 'mdb_server_tests_db2' # Will be deleted in before()

describe 'MDB::ServerApp: MDB::Server requests' do

  before do
    [DB_NAME, DB_NAME_2].each do |name|
      MDB::Server.delete name if MDB::Server.key? name
    end
    MDB::Server.create(DB_NAME, FixtureView)

    @request  = Rack::MockRequest.new(MDB::ServerApp.new)
    @response = nil
  end

  it 'responds to GET "/databases" with an array' do
    r = get '/databases'
    r.class.must_equal Array
    DB_NAME.must_include r   # must_include is backwards...
  end

  it 'responds to GET /databases/exists/:db appropriately' do
    r = get '/databases'
    r.each do |name|
      r = get "/databases/exists/#{name}"
      r.must_equal true
    end
  end

  it 'responds to POST "/:db" and GET "/:db" correctly' do
    r = post "/#{DB_NAME_2}", { :view_class => :FixtureView }
    get("/databases/exists/#{DB_NAME_2}").must_equal true

  end
end


