# This file represents the Model in the MVC pattern.
#
# This model file is written to illustrate the map / reduce application
# model.
#
# * MDB stores lots of disconnected model graphs, and the queries use a
#   map-reduce strategy to search the disconnected graphs for results.
#
# * The MRI web app has thin controllers and thin views, but very fat model
#   (this file).
#
# == TODO
#
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

  # Returns true iff at least one of this post's tags is named tag_name
  def tagged_with?(tag_name)
    @tags.any? { |tag| tag.matches tag_name }
  end

  #############################################
  #  VIEWS
  #############################################

  class << self
    # This is a view method.  It will be invoked by the client app by
    # sending Data will be a collection of posts that the database gives
    # us.  This version does a search against the entire db of posts, and
    # then reduces.
    def recent(posts)
      posts.values.sort { |a,b| b.timestamp <=> a.timestamp }[0..4] # reverse sort
    end

    # This version just returns @recent_posts array, which is updated every
    # time a new blog post is created.
    def recent_fast(posts)
      @recent_posts || []  # dup for in-maglev queries?
    end

    def tagged_with(posts, tag_name)
      # TODO: Should probably help view writers by doing the projection.
      #       perhaps pass in a query as a block, and it manages the id and
      #       projection.
      posts.inject([]) do |acc, (id,post)|
        acc << post if post.tagged_with? tag_name
        acc
      end
    end

    # MDB::Database calls this whenever a document is added
    def document_added(id, document)
      # Update the list of recent posts (saves searching)
      @recent_posts ||= []
      @recent_posts.unshift document
      @recent_posts.pop if @recent_posts.size > 5
    end
  end
end

# NOTE: Tag used to derive from Array, but MagLev has a bug in unmarshal
# and it can't read an MRI marshaled subclass of Array.  So, for right now,
# we delegate to an array and implement a few extra methods.
#
# In this version (disconnected graph), tags may as well be represented as
# strings or symbols, as they are duplicated and only contain one post.
# The fully connected graph example makes this class worthwile.
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

  def matches(name)
    @name == name.to_s
  end

  def self.find_by_name(tags, name)
    tags.inject([]) do |acc, (id,tag)|
      acc << tag if tag.matches name
      acc
    end
  end
end
