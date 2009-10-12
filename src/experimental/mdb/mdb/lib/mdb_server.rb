require 'rubygems'
require 'sinatra'

require 'txn_wrapper'

require 'mdb/core_extensions'

raise "==== Commit MDB Classes"  unless defined? MDB::Server

Exception.install_debug_block do |e|
  puts "--- #{e} #{e.class}"
  case e
#  when Sinatra::NotFound
#    nil.pause
#   when NoMethodError
#      nil.pause if e.message =~ /symbolize/
   when ArgumentError
    nil.pause # if e.message =~ /Illegal creation of a Symbol/
  end
end

# REST interface to MaglevDB.  Accepts RESTful HTTP requests to access and
# manage the data stored in MDB.  This server uses ruby Marshal as the
# serialization format.
#
# == General Use of REST verbs
#
#  |--------+-----------------------------+-------------------------------|
#  | Verb   | Collection                  | Member                        |
#  |--------+-----------------------------+-------------------------------|
#  | GET    | List members                | Get the member                |
#  | PUT    | Replace collection          | Update member (edit)          |
#  | POST   | Create new entry; return id | unclear                       |
#  | DELETE | Delete entire collection    | Remove member from collection |
#  |--------+-----------------------------+-------------------------------|
#
# == REST API for MDB::Server, a collection:
#
#  |--------+-------+---------------------------------------+-------------------|
#  | Verb   | Route | Action [params]                       | Result            |
#  |--------+-------+---------------------------------------+-------------------|
#  | GET    | /     | List database names                   | Array of strings  |
#  | PUT    | /     | NOT SUPPORTED                         |                   |
#  | POST   | /     | Database.create [db_name, view_class] | new id comes back |
#  | DELETE | /     | NOT SUPPORTED                         |                   |
#  |        |       |                                       |                   |
#  | GET    | /:db  | Test if db exists                     | boolean           |
#  | PUT    | /:db  | NOT SUPPORTED                         |                   |
#  | POST   | /:db  | Create new document                   | id                |
#  | DELETE | /:db  | Delete db                             |                   |
#  |--------+-------+---------------------------------------+-------------------|
#
# == REST API for MDB::Database, a collection:
#
#  |--------+-------------------+------------------------------+------------------|
#  | Verb   | Route             | Action                       | View             |
#  |--------+-------------------+------------------------------+------------------|
#  | GET    | /:db/:id          | Get document with :id        | serialized objec |
#  | PUT    | /:db/:id          | Update document :id          | status           |
#  | POST   | /:db/:id          | NOT SUPPORTED                |                  |
#  | DELETE | /:db/:id          | Delete document :id from :db | status           |
#  |        |                   |                              |                  |
#  | GET    | /:db/view/:name   | Run the view                 | data from view   |
#  | GET    | /:db/send/:method | Send :method to ViewClass    | For testing      |
#  |--------+-------------------+------------------------------+------------------|
#
#
# == The server's use of HTTP status codes
#
#  |--------+-------------+---------------------------------------------------|
#  | Status | Name        | Meaning                                           |
#  |--------+-------------+---------------------------------------------------|
#  |    400 | Bad Request | A parameter was missing or incorrect              |
#  |--------+-------------+---------------------------------------------------|
#  |    404 | Not Found   | The resource was not found, e.g., a 404 on        |
#  |        |             | GET /foo/12 indicates no document with id 12      |
#  |        |             | in database foo, or there was no database foo     |
#  |--------+-------------+---------------------------------------------------|
#  |    403 | Forbidden   | The HTTP method (GET, POST,...) was inappropriate |
#  |        |             | for the resource.                                 |
#  |--------+-------------+---------------------------------------------------|
#
# == TODO
#
# * If an exception is thrown, then the content-type and
#   content-transfer-encoding are left at application/mdb and binary
#
# * parameterize the serializer, but I can't seem to get
#   the combination of:
#   1: Passing parameters to the MDB::ServerApp.new(:marshal)
#   2: Starting from a rackup file with an app derived from Sinatra::Base
#   3: passing parameters like :host and :port into the appropriate level.
#   Until then, I'll just hardcode the MarshalSerializer.
#
class MDB::ServerApp < Sinatra::Base

#   require 'log_headers'
#   use LogHeaders

  use MagLevTransactionWrapper
  set :raise_errors, false  # Otherwise, error blocks don't work

  def initialize
    super
    @serializer = MDB::MarshalSerializer.new
    @server = MDB::Server
  end

  before do
    content_type @serializer.content_type  # Handle response type
    # Unpack application/mdb data
    case request.content_type
    when %r{application/mdb}
      @post_data = @serializer.deserialize request.body
    else
      @post_data = request.POST.dup
    end
    @post_data.symbolize_keys if Hash === @post_data
  end

  # This is here, rather than in the before block, since params does not
  # have the path info split out until after the before block is run.
  # Furthermore, not all paths will need the db.
  def get_db
    db_name = params[:db] || @post_data[:db_name]
    halt 400, "No :db or :db_name param" if db_name.nil?
    db = @server[db_name]
    halt 404, "No such Database: #{params[:db]}" if db.nil?
    db
  end

  # May need to wrap all of these in transactions.  At least need to abort
  # before handling most requests, since the DB may be updated from Rake or
  # another server.

  # List db names
  get '/' do
    begin
      @serializer.serialize(@server.db_names)
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # Create a new database
  post '/' do
    # Use strings, not symbols, for @post_data
    result = @server.create(@post_data[:db_name], @post_data[:view_class])
    @serializer.serialize(result.name) # send back just the db name
  end

  # Query if db exists
  get '/:db' do
    begin
      @serializer.serialize(@server.key? params[:db])
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # Create new document
  # Create a new document for the database from form data.
  # The database will add the object to its collection, and then
  # call the model added(new_object) hook.
  post '/:db' do
    begin
      @serializer.serialize get_db.add(@post_data)
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # Delete database
  # The server will delete the database.
  delete '/:db' do
    begin
      get_db # will raise 404 if :db not found
      @serializer.serialize(@server.delete params[:db])
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # Get a document
  get '/:db/:id' do
    begin
      @serializer.serialize(get_db.get(params[:id].to_i))
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # Update a document
#   put '/:db/:id' do
#     @serializer.serialize get_db.put(params[:id].to_i, data)
#   end

  # Delete a document
  delete '/:db/:id' do
    begin
      @serializer.serialize(get_db.delete params[:id].to_i)
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # Run a view on the database.
  # Returns view data.
  # On error, returns appropriate HTTP status code and error message.
  get '/:db/view/:view' do
    begin
      @serializer.serialize(get_db.execute_view(params[:view]))
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  # For testing...
  get '/:db/send/:method' do
    begin
      @serializer.serialize(get_db.send(params[:method].to_sym))
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
  end

  error ArgumentError do
    e = request.env['sinatra.error']
    msg = e.message
    msg << e.backtrace.join("\n")
    halt 400, msg
  end

  error MDB::Server::DatabaseNotFound do
    halt 404, request.env['sinatra.error'].message
  end

  error MDB::MDBError do
    halt 400, request.env['sinatra.error'].message
  end

  error do
    err = request.env['sinatra.error']
    "MDB::ServerApp error: #{request.env['sinatra.error'].message}"
  end

  not_found do
    "MDB:ServerApp: unknown URL: #{request.path} Parameters: #{params.inspect}"
  end
end
