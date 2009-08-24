# This file defines the blog classes.  To commit the code, load this
# file from within a Maglev.persistent block and then commit it.

module Maglev::Model

  module ClassMethods

    def new(*params)
      obj = allocate
      obj.initialize(*params)
      add(obj)
    end

    # Returns an array of all the posts
    def all
      Maglev::PERSISTENT_ROOT[self].values
    end

    def get(id)
      Maglev::PERSISTENT_ROOT[self][id.to_i]
    end

    def add(obj)
      Maglev::PERSISTENT_ROOT[self][obj.__id__] = obj
    end
  end

  def self.included(host)
    Maglev::PERSISTENT_ROOT[host] = Hash.new
    host.extend(ClassMethods)
  end

end

class Post
  include Maglev::Model

  attr_reader :text, :title
  def initialize(params)
    @title = params[:title]
    @text =  params[:text]
  end
end

class Tag < Array
  include Maglev::Model

  attr_reader :name
  def initialize(name)
    @name = name
  end
end

