# This file defines the blog classes.  To commit the code, load this
# file from within a Maglev.persistent block and then commit it.


# Maglev::Model implements a very simple and un-production-worthy
# persistence model.  A class that includes the Maglev::Model module, will
# automatically persist all new instances in a hash table for the class.
# If the module is re-included (or the file that defines the class is
# re-run), then all previous saved instances will be lost.
module Maglev::Model

  # These methods will be defined as class methods of the class that
  # includes this module.
  module ClassMethods

    # Create a new instance, and add it to the hash of persisted instances
    # for this class.
    def new(*params)
      obj = allocate
      obj.initialize(*params)
      add(obj)
    end

    # Returns an array of all the posts
    def all
      Maglev::PERSISTENT_ROOT[self].values
    end

    # Get an object by object id.  If +id+ is not the id for an instance of
    # the calling class, then no object will be found (even if +id+ is the
    # valid id of soome instance of another class).
    def get(id)
      Maglev::PERSISTENT_ROOT[self][id.to_i]
    end

    # Add +obj+ to the set of persisted objects for this class.
    # called by new()
    def add(obj)
      Maglev::PERSISTENT_ROOT[self][obj.__id__] = obj
    end
  end

  def self.included(host)
    # TODO: Almost certainly not a good thing beyond the dev env: This will
    # blow away any persistent data from previous runs.  Perhaps we need a
    # migration idiom so that this can be controlled.
    Maglev::PERSISTENT_ROOT[host] = Hash.new
    host.extend(ClassMethods)
  end

end

class Post
  include Maglev::Model

  attr_reader :text, :title, :timestamp, :tags
  def initialize(params)
    @title = params[:title]
    @text =  params[:text]
    @timestamp = Time.now
    @tags = []
  end
end

class Tag < Array
  include Maglev::Model

  attr_reader :name
  def initialize(name)
    @name = name.to_s
  end
  def to_s
    @name
  end
end
