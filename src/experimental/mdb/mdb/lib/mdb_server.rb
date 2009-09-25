require 'rubygems'
require 'sinatra'
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
class MDB::ServerApp < Sinatra::Base
#   set :host, 'localhost'
#   set :port, 4567


  def initialize(*args)
    super
    @server = MDB::Server
  end

  # Or, we could do specific handlers such as:
  #    error MDB::DatabaseNotFound do
  #       # ...something specific for db not found errors
  #    end
  error do
    "MDB::ServerApp error: #{request.env['sinatra.error'].message}"
  end


  # May need to wrap all of these in transactions.  At least need to abort
  # before handling most requests, since the DB may be updated from Rake or
  # another server.

  # Run a view on the database.
  # Returns view data.
  # On error, returns appropriate HTTP status code and error message.
  get '/:db/view/:view' do
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?

#    Maglev.abort_transaction
    db.execute_view(params[:view])
  end

  # Create a new object for the database from form data.
  # The database will add the object to its collection, and then
  # call the model added(new_object) hook.
  put '/:db' do
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?

    #    Maglev.abort_transaction
    db.add(request.body)
  rescue Exception => e
    halt 500, "Error: #{e}"
  end

#   # TODO: What is the correct HTTP status for a failed creation?
#   put '/post' do
#     Maglev.abort_transaction
#     begin
#       post = Post.new(params)
#       Maglev.commit_transaction
#       post.to_json
#     rescue Exception => e
#       Maglev.abort_transaction
#       halt 406, e.msg
#     end
#   end
end
