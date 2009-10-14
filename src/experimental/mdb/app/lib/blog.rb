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

  # Takes a hash of params => value mappings.  Recognized params:
  # 1. :title      Expects a string
  # 2. :text       Expects a string
  # 3. :timestamp  Expects a Time
  # 4. :tags       Expects an array of strings or symbols
  def initialize(params)
    # TODO: Here we see an opportunity to add ActiveRecord style
    # validations and default values. OTOH, perhaps its more typing to add
    # the validations etc. than to just hard code it the old way?
    @title = params[:title]                      # must_not_be_nil
    @text =  params[:text]                       # must_not_be_nil
    @timestamp = params[:timestamp] || Time.now  # defaults
    @tags = params[:tags] || []
  end

  def tag(*tags)
    tags.each do |tag|
      tag << self
      @tags << tag
    end
  end

  # Returns true iff one of this post's tags is named tag_name
  def tagged_with?(tag_name)
    tn = tag_name.to_s
    !!@tags.detect { |tag| tag.name.to_s == tn }
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

  class << self
    FIVE_DAYS = 5 * 60 * 60 * 24 # Number of seconds in five days

    # This is a view method.  It will be invoked by the client app by sending
    # Data will be a collection of posts that the database gives us.
    def recent(posts)
      cutoff = Time.now - FIVE_DAYS
      recent = posts.select { |k,v| v.timestamp > cutoff }
      recent = recent[0...5].map { |x| x[1] }
      recent.sort {|a,b| b.timestamp <=> a.timestamp } # reverse by time
    end

    def tagged_with(posts, tag_name)
      # Need to be careful sym vs string
      # TODO: Should probably help view writers by doing the project.
      #   perhaps pass in a query as a block, and it manages the id and projection.
      tn = tag_name.to_s
      posts.select { |id,post| post.tagged_with? tn }.map { |x| x[1] }
    end
  end
end

# NOTE: Tag used to derive from Array, but MagLev has a bug in unmarshal
# and it can't read an MRI marshaled subclass of Array.  So, for right now,
# we delegate to an array and implement a few extra methods.
class Tag
  attr_reader :name

  def initialize(name)
    @name = name.to_s
    @tagged = Array.new
  end

  def to_s
    @name
  end

  def <<(tagged_item)
    @tagged << tagged_item
  end

  def each(&block)
    @tagged.each(&block)
  end

  def self.find_by_name(tags, name)
    tn = name.to_s
    tags.inject([]) { |acc,(id,tag)| acc << tag if tag.name == tn; acc }
  end
end
