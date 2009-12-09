require 'rubygems'
require 'sinatra'
require 'txn_wrapper'
require 'mdb/core_extensions'
require 'migration/migration'  # TODO: persist?

raise "==== MDB Classes not committed" unless defined? MDB::Server
raise "==== MDB root not set" if MDB::Server.server.nil?

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
#  | POST   | /:db/view/:name   | Run the view                 | data from view   |
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

class MDB::ServerApp < Sinatra::Base

  use MagLevTransactionWrapper
  set :raise_errors, false  # Otherwise, error blocks don't work

  def initialize
    super
    @serializer = MDB::MarshalSerializer.new
    @server = MDB::Server.server
  end

  before do
    content_type @serializer.content_type  # Handle response type
    # Unpack application/mdb data
    #puts "==== before: #{request.env['REQUEST_METHOD']}  #{request.env['PATH_INFO']}"
    @post_hash = nil
    @post_data = nil
    case request.content_type
    when %r{application/mdb}
      @post_data = @serializer.deserialize(request.body)
      @post_data.each { |el| el.symbolize_keys if Hash === el }
    else
      @post_hash = request.POST.dup
      @post_hash.symbolize_keys
    end
  end

  # This is here, rather than in the before block, since params does not
  # have the path info split out until after the before block is run.
  # Furthermore, not all paths will need the db.
  def get_db
    db_name = params[:db] || @post_hash[:db_name]
    halt 400, "No :db or :db_name param" if db_name.nil?
    db = @server[db_name]
    halt 404, "No such Database: #{params[:db]}" if db.nil?
    db
  end

  get '/debug_info' do
    @serializer.serialize(@server.debug_info)
  end

  # List db names
  get '/' do
    @serializer.serialize(@server.db_names)
  end

  # Create a new database
  post '/' do
    h = @post_data[0]
    result = @server.create(h[:db_name], h[:view_class])
    @serializer.serialize(result.name) # send back just the db name
  end

  post '/migrate' do
    h = @post_data[0]
    klass = h[:klass]
    raise ArgumentError, "No :klass for migrate." unless klass
    ruby_source = h[:ruby_source]
    raise ArgumentError, "No :ruby_source for migrate." unless klass
    migrate_instances = h[:migrate_instances]
    raise ArgumentError, "No :migrate_instances for migrate." unless klass

    Maglev::Migration.migrate(klass, ruby_source, migrate_instances)
    @serializer.serialize true
  end

  # Query if db exists
  get '/:db' do
    @serializer.serialize(@server.key? params[:db])
  end

  # Create new document
  # Create a new document for the database from form data.
  # The database will add the object to its collection, and then
  # call the model added(new_object) hook.
  post '/:db' do
    @serializer.serialize(get_db.add(@post_data[0]))
  end

  # Delete database
  # The server will delete the database.
  delete '/:db' do
    get_db       # will raise 404 if :db not found
    @serializer.serialize(@server.delete params[:db])
  end

  # Get a document
  get '/:db/:id' do
    @serializer.serialize(get_db.get(params[:id].to_i))
  end

  # Delete a document
  delete '/:db/:id' do
    @serializer.serialize(get_db.delete params[:id].to_i)
  end

  # Run a view on the database.
  # Returns view data.
  # On error, returns appropriate HTTP status code and error message.
  post '/:db/view/:view' do
    # puts "-- POST '/params[:db]/view/params[:view]: @post_data: #{@post_data.inspect}"
    args = [params[:view]]
    args = args + @post_data unless @post_data.nil?
    # puts "-- POST '/params[:db]/view/params[:view]: @args: #{@args.inspect}"
    @serializer.serialize(get_db.execute_view(*args))
  end

  # For testing...
  get '/:db/send/:method' do
    @serializer.serialize(get_db.send(params[:method].to_sym))
  end

  error ArgumentError do
    halt 400, err_msg
  end

  error MDB::Server::DatabaseNotFound do
    halt 404, err_msg
  end

  error MDB::MDBError do
    halt 400, err_msg
  end

  error do
    err_msg
  end

  not_found do
    err_msg "unknown URL: #{request.path} Parameters: #{params.inspect}"
  end

  def err_msg(msg=nil)
    m = 'MDB::ServerApp error: '
    m << msg unless msg.nil?
    m << "\n#{request.env['REQUEST_METHOD']}  #{request.env['PATH_INFO']}\n"
    e = request.env['sinatra.error']
    if e.nil?
      m << "  (No exception available)\n"
    else
      m << "  #{e.class} #{e.inspect}\n"
      m << e.message if e.respond_to? :message
      m << e.backtrace.join("\n")
    end
    m
  end
end

