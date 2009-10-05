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

Exception.install_debug_block do |e|
  case e
  when ArgumentError, NoMethodError
    nil.pause # if e.message =~ /to_sym/
  end
end

# TODO: MDB::ServerApp only supports Marshal right now...
SERIALIZER_CLASS =
  case ARGV[0]
  when 'marshal'
    MDB::MarshalSerializer
  when 'json'
    MDB::JSONSerializer.new
  else
    raise "Uknown serialization format #{ARGV[0]}"
  end
SERIALIZER = SERIALIZER_CLASS.new

# Get path wrapped in a transaction.  Make sure the status is success.
# status may be either a range or a Fixnum.
def get(path, status=(200..299))
  # Since the real app will be running in a Rack stack with a transaction
  # wrapper around each HTTP request, in this test code, we manually wrap
  # the transactions to simulate the rack txn wrapper.
  Maglev.transaction { @response = @request.get(path) }
  handle_response(status)
end

def post_urlencode(path, expected_status, data={ })
  # This only works for simple sets of parameters. Send data as urlencoded
  # form.

  p = data.inject("") { |acc, (key, value)| acc << "#{key}=#{value}&" }.chop
  e = {
    :input => p,
    "CONTENT_LENGTH" => p.size,
    "CONTENT_TYPE"   => 'application/x-www-form-urlencoded'
  }
  Maglev.transaction { @response = @request.post(path, e) }
  handle_response(expected_status)
end

def post_serialized(path, data='')
  s_data = SERIALIZER.serialize data
  e = {
    :input => s_data,
    "CONTENT_LENGTH" => s_data.size,
    "CONTENT_TYPE"   => SERIALIZER.content_type
  }

  Maglev.transaction { @response = @request.post(path, e) }
  handle_response
end

def delete(path, status=(200..299))
  Maglev.transaction { @response = @request.delete(path) }
  handle_response(status)
end

# Decode serialized data from response, if success status code (2xx)
def handle_response(status=200)
  case status
  when Range
    s = status.include?(@response.status)
    s.must_equal true
  else
    @response.status.must_equal status
  end

  if (200..299).include? @response.status
    obj = SERIALIZER.deserialize @response.body
  else
    nil
  end
end

DB_NAME   = :mdb_server_tests_db  # Will be created fresh in before()
DB_NAME_2 = :mdb_server_tests_db2 # Will be deleted in before()

describe 'MDB::ServerApp: MDB::Server requests' do

  before do
    [DB_NAME, DB_NAME_2].each do |name|
      MDB::Server.delete name if MDB::Server.key? name
    end
    MDB::Server.create(DB_NAME, AppModel)

    @request  = Rack::MockRequest.new(MDB::ServerApp.new)
    @response = nil
  end


=begin

These requests correspond to methods on MDB::Server, a collection:

  |--------+-------+---------------------------------------+-------------------------|
  | Verb   | Route | Action [params]                       | Tested?                 |
  |--------+-------+---------------------------------------+-------------------------|
  | GET    | /     | List database names                   | yes                     |
  | PUT    | /     | NOT SUPPORTED                         | N/A                     |
  | POST   | /     | Database.create [db_name, view_class] | yes: urlencode version  |
  | DELETE | /     | NOT SUPPORTED                         | N/A                     |
  |        |       |                                       |                         |
  | GET    | /:db  | Test if db exists                     | yes                     |
  | PUT    | /:db  | NOT SUPPORTED                         | N/A                     |
  | POST   | /:db  | Create new document                   | yes: serialized version |
  | DELETE | /:db  | Delete db                             |                         |
  |--------+-------+---------------------------------------+-------------------------|

These requests correspond to methods on MDB::Database

  |--------+-------------------+------------------------------+------------------|
  | Verb   | Route             | Action                       | View             |
  |--------+-------------------+------------------------------+------------------|
  | GET    | /:db/:id          | Get document with :id        | serialized objec |
  | PUT    | /:db/:id          | Update document :id          | status           |
  | POST   | /:db/:id          | NOT SUPPORTED                |                  |
  | DELETE | /:db/:id          | Delete document :id from :db | status           |
  |        |                   |                              |                  |
  | GET    | /:db/view/:name   | Run the view                 | data from view   |
  | GET    | /:db/send/:method | Send :method to ViewClass    | For testing      |
  |--------+-------------------+------------------------------+------------------|
=end

  # General gets
  it 'responds to GET "/#{DB_NAME}/Does/Not/Exist" with a 404' do
    r = get "/#{DB_NAME}/Does/Not/Exist", 404
  end

  it 'responds to get "/not_a_db_name" with false' do
    r = get "/not_a_db_name"
    r.must_equal false
  end

  it 'responds to GET "/" with an array' do
    r = get '/'
    r.class.must_equal Array
    DB_NAME.must_include r   # must_include is backwards...
  end

  it 'responds to GET "/:db" with a boolean for databases that exist' do
    names = get '/'
    names.each do |name|
      r = get "/#{name}"
      r.must_equal true
    end
    # TODO: Add test for not found db
 end

  # TODO: Should probably expect 201, not 200 from a successful POST
  it 'responds to POST "/" and GET "/:db" correctly' do
    r = post_urlencode "/", 200, { :db_name => DB_NAME_2,
                                   :view_class => :AppModel }
    get("/#{DB_NAME_2}").must_equal true

    # TODO: Probably need to test that the model class got installed
    # correctly.  Only way to do that is to est if the view methods are
    # correct.
  end

  it 'responds to POST "/" with 404 if missing :view_name' do
    r = post_urlencode "/", 404, { :db_name => DB_NAME_2 }
  end

  it 'responds to POST "/" with 404 if missing :db_name' do
    r = post_urlencode "/", 404, { :view_name => :AppClass }
  end

  it 'responds to POST "/" with 404 if :view_class is bad' do
    r = post_urlencode "/", 404, { :db_name => DB_NAME_2,
                                   :view_class => :NotAClass }
  end

  it 'responds to DELETE "/:db" by deleting the db if it exists' do
    r = delete "/#{DB_NAME}"
    r.must_equal true

    # After the delete, shouldn't be able to get it
    r = get "/#{DB_NAME}"
    r.must_equal false
  end

  it 'responds to DELETE "/:db" by returning 404 if :db does not exist' do
    r = delete "/this_is_not_a_db_name", 404
  end
end

describe 'MDB::ServerApp: MDB::Database requests' do
  before do
    [DB_NAME, DB_NAME_2].each do |name|
      MDB::Server.delete name if MDB::Server.key? name
    end
    @db = MDB::Server.create(DB_NAME, AppModel)
    @doc1 = AppModel.new(1, 2)
    @doc1_id = @db.add(@doc1)
    @request  = Rack::MockRequest.new(MDB::ServerApp.new)
    @response = nil
  end

#   it 'responds to GET "/:db/:id" by returning the to_json of the document' do
#     doc = get "/#{DB_NAME}/#{@doc1_id}"
#     doc.wont_be_nil
#     doc['a'].must_equal 1
#     doc['b'].must_equal 2
#   end

  it 'responds to POST "/:db" by creating a new document and returning the id' do
    id = post_serialized "/#{DB_NAME}", AppModel.new(3,4)
    id.wont_be_nil

    doc = get "/#{DB_NAME}/#{id}"
    doc.wont_be_nil
    doc.a.must_equal 3
    doc.b.must_equal 4
  end

#   it 'responds to POST "/:db" and GET "/:db" correctly' do
#     r = post "/#{DB_NAME_2}", { :view_class => :AppModel }
#     get("/#{DB_NAME_2}").must_equal true

#     # TODO: Probably need to test that the model class got installed
#     # correctly.  Only way to do that is to est if the view methods are
#     # correct.
#   end

end
