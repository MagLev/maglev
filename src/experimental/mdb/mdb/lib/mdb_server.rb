require 'rubygems'
require 'sinatra'

raise "==== Commit MDB Classes"  unless defined? MDB::Server

# Exception.install_debug_block do |e|
#   puts "--- #{e} #{e.class}"
#   case e
#   when RuntimeError
#     nil.pause if e.message =~ /Illegal creation of a Symbol/
#   end
# end

# REST interface to MaglevDB.  Accepts RESTful HTTP requests to access and
# manage the data stored in MDB.  This server uses ruby Marshal as the
# serialization format.

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

  |--------+-------+---------------------+-------------------|
  | Verb   | Route | Action              | View              |
  |--------+-------+---------------------+-------------------|
  | GET    | /     | List database names | Array of strings  |
  | PUT    | /     | NOT SUPPORTED       |                   |
  | POST   | /     | Database.create     | new id comes back |
  | DELETE | /     | NOT SUPPORTED       |                   |
  |        |       |                     |                   |
  | GET    | /:db  | Test if db exists   | boolean           |
  | PUT    | /:db  | NOT SUPPORTED       |                   |
  | POST   | /:db  | Create new document | id                |
  | DELETE | /:db  | Delete db           |                   |
  |--------+-------+---------------------+-------------------|

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

  def initialize(serializer, *args)
    super(*args)
    raise ArgumentError if serializer.nil?
    @serializer = serializer
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
    @serializer.serialize @server.db_names
  end

  # Create a new database
  post '/' do
    @serializer.serialize @server.create(params[:db_name], params[:view_class])
  end

  # Query if db exists
  get '/:db' do
    @serializer.serialize @server.key? params[:db]
  end

  # Create new document
  # Create a new document for the database from form data.
  # The database will add the object to its collection, and then
  # call the model added(new_object) hook.
  post '/:db' do
    # .string is needed since request may be a StringIO,
    # and StringIO may not be committed to the repository.
    obj = @serializer.deserialize(request.body.string)
    @serializer.serialize get_db.add(obj)
  end

  # Delete database
  # The server will delete the database.
  delete '/:db' do
    # .string is needed since request may be a StringIO,
    # and StringIO may not be committed to the repository.
    @serializer.serialize @server.delete params[:db]
  end

  # Get a document
  get '/:db/:id' do
    @serializer.serialize get_db.get(params[:id].to_i)
  end

  # Update a document
#   put '/:db/:id' do
#     @serializer.serialize get_db.put(params[:id].to_i, data)
#   end

  # Delete a document
  delete '/:db/:id' do
    @serializer.serialize get_db.delete params[:id].to_i
  end

  # Run a view on the database.
  # Returns view data.
  # On error, returns appropriate HTTP status code and error message.
  get '/:db/view/:view' do
    @serializer.serialize get_db.execute_view(params[:view])
  end

# TODO: /mdb is top level URL and represents the collection of dbs
#   POST /mdb  Create new db
  # For testing...
  get '/:db/send/:method' do
    @serializer.serialize get_db.send(params[:method].to_sym)
  end

  # Or, we could do specific handlers such as:
  #    error MDB::DatabaseNotFound do
  #       # ...something specific for db not found errors
  #    end
  error do
    "MDB::ServerApp error: #{request.env['sinatra.error'].message}"
  end

  not_found do
    "MDB:ServerApp: unknown URL: #{request.path}"
  end
end
