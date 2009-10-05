require 'rubygems'
require 'sinatra'

require 'txn_wrapper'

raise "==== Commit MDB Classes"  unless defined? MDB::Server

# Exception.install_debug_block do |e|
#   puts "--- #{e} #{e.class}"
#   case e
#   when NoMethodError
#     nil.pause if e.message =~ /gsub/
#   when ArgumentError
#     nil.pause # if e.message =~ /Illegal creation of a Symbol/
#   end
# end

# REST interface to MaglevDB.  Accepts RESTful HTTP requests to access and
# manage the data stored in MDB.  This server uses ruby Marshal as the
# serialization format.
#
# For all URLs of the form /:db, it should return 404 not found if
# the db does not exist.
#
=begin

  |--------+-----------------------------+-------------------------------|
  | Verb   | Collection                  | Member                        |
  |--------+-----------------------------+-------------------------------|
  | GET    | List members                | Get the member                |
  | PUT    | Replace collection          | Update member (edit)          |
  | POST   | Create new entry; return id | unclear                       |
  | DELETE | Delete entire collection    | Remove member from collection |
  |--------+-----------------------------+-------------------------------|

These requests correspond to methods on MDB::Server, a collection:

  |--------+-------+---------------------------------------+-------------------|
  | Verb   | Route | Action [params]                       | Result            |
  |--------+-------+---------------------------------------+-------------------|
  | GET    | /     | List database names                   | Array of strings  |
  | PUT    | /     | NOT SUPPORTED                         |                   |
  | POST   | /     | Database.create [db_name, view_class] | new id comes back |
  | DELETE | /     | NOT SUPPORTED                         |                   |
  |        |       |                                       |                   |
  | GET    | /:db  | Test if db exists                     | boolean           |
  | PUT    | /:db  | NOT SUPPORTED                         |                   |
  | POST   | /:db  | Create new document                   | id                |
  | DELETE | /:db  | Delete db                             |                   |
  |--------+-------+---------------------------------------+-------------------|

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
#
# This class will ensure that all MDB level responses have properly
# serialized (either in JSON or Marshal, depending on the serializer object
# we are customized with).
class MDB::ServerApp < Sinatra::Base

#  require 'log_headers'
#  use LogHeaders

  use MagLevTransactionWrapper

  def initialize
    super
    # TODO: I need to parameterize the serializer, but I can't seem to get
    # the combination of:
    # 1: Passing parameters to the MDB::ServerApp.new(:marshal)
    # 2: Starting from a rackup file with an app derived from Sinatra::Base
    # 3: passing parameters like :host and :port into the appropriate level.
    # Until then, I'll just hardcode the MarshalSerializer.
    @serializer = MDB::MarshalSerializer.new
    @server = MDB::Server
  end

  before do
    Maglev.abort_transaction  # refresh view of db # TODO: Is this ok?
    content_type @serializer.content_type
  end

  # This is here, rather than in the before block, since params does not
  # have the path info split out until after the before block is run.
  # Furthermore, not all paths will need the db.
  def get_db
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?
    db
  end

  # May need to wrap all of these in transactions.  At least need to abort
  # before handling most requests, since the DB may be updated from Rake or
  # another server.

  # List db names
  get '/' do
    @serializer.serialize(@server.db_names)
  end

  # Create a new database
  post '/' do
    # TODO: I tried to setup an error handler for MDB::MDBError, but two problems:
    # 1: The thrown exception's class is matched exactly, not with ===, so you can't
    #    setup a handler for a tree of exceptions
    # 2: halt 404, didn't seem to work from in there...
    begin
      result = @server.create(params[:db_name], params[:view_class])
      @serializer.serialize(result.name) # send back just the db name
    rescue MDB::MDBError => e
      halt 404, "MDB::ServerApp error: #{e.message}"
    end
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
    # .string is needed since request may be a StringIO
    obj = @serializer.deserialize(request.body.string)
    @serializer.serialize(get_db.add(obj))
  end

  # Delete database
  # The server will delete the database.
  delete '/:db' do
    get_db # will raise 404 if :db not found
    @serializer.serialize(@server.delete params[:db])
  end

  # Get a document
  get '/:db/:id' do
    @serializer.serialize(get_db.get(params[:id].to_i))
  end

  # Update a document
#   put '/:db/:id' do
#     @serializer.serialize get_db.put(params[:id].to_i, data)
#   end

  # Delete a document
  delete '/:db/:id' do
    @serializer.serialize(get_db.delete params[:id].to_i)
  end

  # Run a view on the database.
  # Returns view data.
  # On error, returns appropriate HTTP status code and error message.
  get '/:db/view/:view' do
    @serializer.serialize(get_db.execute_view(params[:view]))
  end

  # For testing...
  get '/:db/send/:method' do
    @serializer.serialize(get_db.send(params[:method].to_sym))
  end

  error do
    "MDB::ServerApp error: #{request.env['sinatra.error'].message}"
  end

  not_found do
    "MDB:ServerApp: unknown URL: #{request.path} Parameters: #{params.inspect}"
  end
end
