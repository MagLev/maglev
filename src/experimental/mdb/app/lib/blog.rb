# This file represents the Model in the MVC pattern.
#
# The file will be loaded into MagLev as an MDB Model for the Database
# named 'post' (see Rakefile mdb:create).  The class methods in Post will
# be available to the Sinatra app as views from the MDB::Server.
class Post

  attr_reader :text, :title, :timestamp, :tags
  def initialize(params)
    @title = params[:title]
    @text =  params[:text]
    @timestamp = Time.now
    @tags = []
  end

  # Tag the post: (a) adds reciever to the tag and (b) adds
  # each tag to recevier's @tags
  def tag(*tags)
    p tags
    tags.each do |tag|
      tag << self
      @tags << tag
    end
  end

  # This is a view method.  It will be invoked by the client app by sending
  # a /post/recent URL to the MDB::ServerApp.
  def self.recent
    # JSON, handcrafted with Emacs, using old world techniques
    seconds_per_day = 24 * 60 * 60
    %Q<
      [
        {"title":"Blog Post the second",
         "timestamp":"#{Time.now - (3 * seconds_per_day)}",
         "text":"This is a really long blog post text",
         "id":"xyzzy1",
         "tags":["maglev"]},
        {"title":"The first blog post",
         "timestamp":"#{Time.now}",
         "text":"This is a really long blog post text",
         "id":"xyzzy",
         "tags":["maglev", "blogs"]},
        {"title":"Blog Post the third",
         "timestamp":"#{Time.now - (4 * seconds_per_day)}",
         "text":"This is a really long blog post text",
         "id":"xyzzy2",
         "tags":[]}
      ]
      >
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
