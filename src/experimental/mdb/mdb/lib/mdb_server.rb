require 'rubygems'
require 'sinatra'
# require 'fake_json'

raise "==== Commit MDB Classes"  unless defined? MDB::Server


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

  get '/:db/:view' do
    Maglev.abort_transaction
    db = @server[params[:db]]
    halt 404, "No such Database: #{params[:db]}" if db.nil?

    db.execute_view(params[:view])
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
