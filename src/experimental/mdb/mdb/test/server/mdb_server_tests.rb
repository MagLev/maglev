# MiniTest suite for the MDB::Server using Marshal
#
# NOTE: Rack::Test currently does not work in MagLev, so we're using raw
# Rack::MockRequest and Rack::MockResponse.

require 'rubygems'
require 'minitest/spec'
require 'maglev/maglev_json'
require 'rack'

require 'mdb_server'

MiniTest::Unit.autorun

# Exception.install_debug_block do |e|
#   case e
#   when NoMethodError
#     nil.pause # if e.message =~ /to_sym/
#   end
# end

# Get path wrapped in a transaction.  Make sure the status is success.
# status may be either a range or a Fixnum.
def get(path, status=(200..299))
  # Since the real app will be running in a Rack stack with a transaction
  # wrapper around each HTTP request, in this test code, we manually wrap
  # the transactions to simulate the rack txn wrapper.
  Maglev.transaction { @response = @request.get(path) }
  handle_response(status)
end

def post_urlencode(path, data={ })
  # This only works for simple sets of parameters. Send data as urlencoded
  # form.

  p = data.inject("") { |acc, (key, value)| acc << "#{key}=#{value}&" }.chop
  e = {
    :input => p,
    "CONTENT_LENGTH" => p.size,
    "CONTENT_TYPE"   => 'application/x-www-form-urlencoded'
  }
  Maglev.transaction { @response = @request.post(path, e) }
  handle_response
end

def post_json(path, data='')
  e = {
    :input => data,
    "CONTENT_LENGTH" => data.size,
    "CONTENT_TYPE"   => 'application/json'
  }

  Maglev.transaction { @response = @request.post(path, e) }
  handle_response
end

# Decode JSON from response, if success status code (2xx)
def handle_response(status=200)
  case status
  when Range
    s = status.include?(@response.status)
    s.must_equal true
  else
    @response.status.must_equal status
  end

  if (200..299).include? @response.status
    json = @response.body
#    nil.pause
    x = JSON.parse(@response.body)
#    nil.pause
    x[0]
  else
    nil
  end
end

DB_NAME   = 'mdb_server_tests_db'  # Will be created fresh in before()
DB_NAME_2 = 'mdb_server_tests_db2' # Will be deleted in before()

describe 'MDB::ServerApp: MDB::Server requests' do

  before do
    [DB_NAME, DB_NAME_2].each do |name|
      MDB::Server.delete name if MDB::Server.key? name
    end
    MDB::Server.create(DB_NAME, AppModel)

    @request  = Rack::MockRequest.new(MDB::ServerApp.new)
    @response = nil
  end

  it 'responds to GET "/Does/Not/Exist" with a 404' do
    r = get '/Does/Not/Exist', 404
  end

#   it 'responds to GET "/" with an array' do
#     r = get '/'
#     r.class.must_equal Array
#     DB_NAME.must_include r   # must_include is backwards...
#   end

#   it 'responds to GET "/:db" with a boolean for databases that exist' do
#     names = get '/'
#     names.each do |name|
#       r = get "/#{name}"
#       r.must_equal true
#     end
#     # TODO: Add test for not found db
#  end

#   it 'responds to POST "/" and GET "/:db" correctly' do
#     r = post_urlencode "/", { :db_name => DB_NAME_2,
#                               :view_class => :AppModel }
#     get("/#{DB_NAME_2}").must_equal true

#     # TODO: Probably need to test that the model class got installed
#     # correctly.  Only way to do that is to est if the view methods are
#     # correct.
#   end

end

# describe 'MDB::ServerApp: MDB::Database requests' do
#   before do
#     [DB_NAME, DB_NAME_2].each do |name|
#       MDB::Server.delete name if MDB::Server.key? name
#     end
#     @db = MDB::Server.create(DB_NAME, AppModel)
#     @doc1 = AppModel.new(1, 2)
#     @doc1_id = @db.add(@doc1)
#     @request  = Rack::MockRequest.new(MDB::ServerApp.new)
#     @response = nil
#   end

# #   it 'responds to GET "/:db/:id" by returning the to_json of the document' do
# #     doc = get "/#{DB_NAME}/#{@doc1_id}"
# #     doc.wont_be_nil
# #     doc['a'].must_equal 1
# #     doc['b'].must_equal 2
# #   end

#   it 'responds to POST "/:db" by creating a new document and returning the id' do
#     id = post_json "/#{DB_NAME}", AppModel.new(3,4).to_json
#     id.wont_be_nil

#     doc = get "/#{DB_NAME}/#{id}"
#     doc.wont_be_nil
#     doc['a'].must_equal 3
#     doc['b'].must_equal 4
#   end

# #   it 'responds to POST "/:db" and GET "/:db" correctly' do
# #     r = post "/#{DB_NAME_2}", { :view_class => :AppModel }
# #     get("/#{DB_NAME_2}").must_equal true

# #     # TODO: Probably need to test that the model class got installed
# #     # correctly.  Only way to do that is to est if the view methods are
# #     # correct.
# #   end

# end
