# This file represents the Model in the MVC pattern.
#
# The file will be loaded into MagLev as an MDB Model for the Database
# named 'post' (see Rakefile mdb:create).  The class methods in Post will
# be available to the Sinatra client app as views from the MDB::Server.
#
# == TODO
#
# * Should each data item know their own id?  We could mixin support for
#   adding an id field.
#
class Post
  attr_reader :text, :title, :timestamp, :tags

  # TODO: this field should be managed by MDBModel mixin
  attr_accessor :id

  def initialize(params)
    # TODO: Here we see an opportunity to add ActiveRecord style
    # validations and default values. OTOH, perhaps its more typing to add
    # the validations etc. than to just hard code it the old way?
    @title = params[:title]                      # must_not_be_nil
    @text =  params[:text]                       # must_not_be_nil
    @timestamp = params[:timestamp] || Time.now  # defaults
    @tags = params[:tags] || []                  # defaults
  end

  # MDB::Database calls this whenever a document is added
  def document_added(id, document)
    # Update the list of recent posts (saves searching)
    @recent_posts ||= []
    @recent_posts << document
    @recent_posts.shift if @recent_posts.size > 5
  end

  #############################################
  #  VIEWS
  #############################################

  FIVE_DAYS = 5 * 60 * 60 * 24 # Number of seconds in five days

  # This is a view method.  It will be invoked by the client app by sending
  # Data will be a collection of posts that the database gives us.
  def self.recent(data)
    cutoff = Time.now - FIVE_DAYS
    recent = data.select { |k,v| v.timestamp > cutoff }
    recent[0..5].map { |x| x[1] }
  end
end

