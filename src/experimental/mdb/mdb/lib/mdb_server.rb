require 'rubygems'
require 'sinatra'
require 'iconv'
require 'json'
# require 'fake_json'

raise "==== Commit MDB Classes"  unless defined? MDB::Server

# REST interface to MaglevDB.  Accepts RESTful HTTP requests to access and
# manage the data stored in MDB.
#
# == Database management
#
# GET /mdb         : List the databases in MDB
# PUT /mdb         : Not supported. (should replace all the dbs with a new set)
# POST /mdb        : Add a new database to MDB (not supported: see PUT /mdb/:id)
# PUT /mdb/:id     : Create a new, empty database named by id.
# DELETE /mdb/:id  : Delete the database named by id
#
# == REST methods for a particular database
#
# GET
# PUT
# POST
# DELETE
#
=begin

  |--------+-----------------+-----------------------------+--------------------|
  | Verb   | Route           | Action                      | View               |
  |--------+-----------------+-----------------------------+--------------------|
  | GET    | /:db/view/:name | Run the view                | data from view     |
  |        |                 |                             |                    |
  | GET    | /:db/:id        | Get object :id from :db     | the object as json |
  | PUT    | /:db/:id        | Update object :id  into :db | status             |
  | POST   | /:db            | Database.create             | new id comes back  |
  | DELETE | /:db/:id        | Delete object :id from :db  | status             |
  |--------+-----------------+-----------------------------+--------------------|

=end
#
# This class will ensure that all MDB level responses have properly formed
# JSON bodies.  The views should ensure they return just the subgraph they
# need (or create a hash of data) so that we do not JSON-ize the entire
# repository and send it back as a response...
class MDB::ServerApp < Sinatra::Base

  def initialize(*args)
    super
    @server = MDB::Server
  end

  # May need to wrap all of these in transactions.  At least need to abort
  # before handling most requests, since the DB may be updated from Rake or
  # another server.

  get '/:db/:id' do
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?
    db.get(params[:id].to_i).to_json
  end

  # Run a view on the database.
  # Returns view data.
  # On error, returns appropriate HTTP status code and error message.
  get '/:db/view/:view' do
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?
    db.execute_view(params[:view])
  end

  # Create new document
  # Create a new document for the database from form data.
  # The database will add the object to its collection, and then
  # call the model added(new_object) hook.
  put '/:db' do
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?

    begin
      # .string is needed since request may be a StringIO,
      # and StringIO may not be committed to the repository.
      id = db.add(request.body.string)
      [id].to_json
    rescue Exception => e
      halt 500, "Error: #{e}"
    end
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
