# This file represents the Model in the MVC pattern.
#
# The file will be loaded into MagLev as an MDB Model for the Database
# named 'post' (see Rakefile mdb:create).  The class methods in Post will
# be available to the Sinatra client app as views from the MDB::Server.
class Post
  attr_reader :text, :title, :timestamp, :tags

  def initialize(params)
    # TODO: Here we see an opportunity to add ActiveRecord style
    # validations and default values. OTOH, perhaps its more typing to add
    # the validations etc. than to just hard code it the old way?
    @title = params[:title]                      # must_not_be_nil
    @text =  params[:text]                       # must_not_be_nil
    @timestamp = params[:timestamp] || Time.now  # defaults
    @tags = params[:tags] || []                  # defaults
  end

  # Tag the post: (a) adds reciever to the tag and (b) adds
  # each tag to recevier's @tags
  def tag(*tags)
    tags.each do |tag|
      tag << self
      @tags << tag
    end
  end

  #############################################
  #  VIEWS
  #############################################
  # TODO:
  #   1. Move the views to a Module?


  # This is a view method.  It will be invoked by the client app by sending
  # a /post/recent URL to the MDB::ServerApp.
  def self.recent
    # The JSON gem is not yet working with MagLev, so, this JSON is
    # handcrafted with Emacs, using old world techniques.
    seconds_per_day = 24 * 60 * 60
    [{ "title"     => "The first blog post",
       "timestamp" => "#{Time.now}",
       "text"      => "This is a really long blog post text",
       "id"        => "xyzzy",
       "tags"      => ["maglev", "blogs"]},
     { "title"     => "Blog Post the second",
       "timestamp" => "#{Time.now - (3 * seconds_per_day)}",
       "text"      => "This is a really long blog post text",
       "id"        => "xyzzy1",
       "tags"      => ["maglev"]},
     { "title"     => "Blog Post the third",
       "timestamp" => "#{Time.now - (4 * seconds_per_day)}",
       "text"      => "This is a really long blog post text",
       "id"        => "xyzzy2",
       "tags"      => []}]
  end

  #############################################
  # Stuff to be injected by the database
  #############################################
  class << self
    attr_reader :database
    def set_db(database)
      @database = database
    end
  end
end

class Tag < Array
  attr_reader :name

  def initialize(name)
    @name = name.to_s
  end

  def to_s
    @name
  end

  def self.find_by_name(name)
    Tag.detect { |t| t.name == name }
  end
end
