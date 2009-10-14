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

def get(path)
  # Since the real app will be running in a Rack stack with a transaction
  # wrapper around each HTTP request, in this test code, we manually wrap
  # the transactions to simulate the rack txn wrapper.
  Maglev.transaction { @response = @request.get(path) }
  handle_response
end

def post(path, *data)
  # *data wraps data in an array, which is what we need to send.
  s_data = SERIALIZER.serialize data
  e = {
    :input => s_data,
    "CONTENT_LENGTH" => s_data.size,
    "CONTENT_TYPE"   => SERIALIZER.content_type
    }
  e['CONTENT_TRANSFER_ENCODING'] = 'binary' if SERIALIZER.content_type == 'application/mdb'
  Maglev.transaction { @response = @request.post(path, e) }
  handle_response
end

def delete(path)
  Maglev.transaction { @response = @request.delete(path) }
  handle_response
end

# Decode serialized data from response, if success status code (2xx)
def handle_response
  case @expected_status
  when Range
    s = @expected_status.include?(@response.status)
    s.must_equal true
  else
    @response.status.must_equal @expected_status
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
    @expected_status = 200
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
  | POST   | /:db/view/:name   | Run the view                 | data from view   |
  | GET    | /:db/send/:method | Send :method to ViewClass    | For testing      |
  |--------+-------------------+------------------------------+------------------|
=end

  # General gets
  it 'responds to GET "/#{DB_NAME}/Does/Not/Exist" with a 404' do
    @expected_status = 404
    r = get "/#{DB_NAME}/Does/Not/Exist"
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

  it 'responds to POST "/" and GET "/:db" correctly' do
    r = post "/", { :db_name => DB_NAME_2, :view_class => AppModel }
    get("/#{DB_NAME_2}").must_equal true
  end

  it 'responds to POST "/" with 400 if missing :view_name' do
    @expected_status = 400
    r = post "/", { :db_name => DB_NAME_2 }
  end

  it 'responds to POST "/" with 400 if missing :db_name' do
    @expected_status = 400
    r = post "/", { :view_name => :AppClass }
  end

  it 'responds to POST "/" with 400 if :view_class is bad' do
    @expected_status = 400
    r = post "/", { :db_name => DB_NAME_2,
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
    @expected_status = 404
    r = delete "/this_is_not_a_db_name"
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
    @expected_status = 200
  end

  it 'responds to POST "/:db" by creating a new document and returning the id' do
    id = post "/#{DB_NAME}", AppModel.new(3,4)
    id.wont_be_nil

    doc = get "/#{DB_NAME}/#{id}"
    doc.wont_be_nil
    doc.a.must_equal 3
    doc.b.must_equal 4
  end

  it 'executes views for POST /:db/view/:view (no args)' do
    result = post "/#{DB_NAME}/view/view_42"
    result.must_equal 42
  end

  it 'executes views for POST /:db/view/:view (one arg)' do
    result = post "/#{DB_NAME}/view/view_66", 11
    result.must_equal 77
  end

  it 'executes views for POST /:db/view/:view (two args)' do
    result = post "/#{DB_NAME}/view/view_67", 11, [:foo, :bar]
    result.must_equal 80
  end
end
