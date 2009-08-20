# This file defines the blog classes.  To commit the code, load this
# file from within a Maglev.persistent block and then commit it.
class Post

  # TODO: All of these class methods are fodder for a Maglev::Model
  # module...
  def self.new(params)
    p = allocate
    p.initialize(params)
    add(p)
  end

  # Returns an array of all the posts
  def self.all_posts
    Maglev::PERSISTENT_ROOT[Post].values
  end

  def self.get(id)
    Maglev::PERSISTENT_ROOT[Post][id.to_i]
  end

  def self.add(post)
    Maglev::PERSISTENT_ROOT[Post][post.__id__] = post
  end

  attr_reader :text, :title
  def initialize(params)
    @title = params[:title]
    @text =  params[:text]
  end
end

Maglev::PERSISTENT_ROOT[Post] = Hash.new
