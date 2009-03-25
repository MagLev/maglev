puts "=== READING blog.rb"
class Blog
  attr_reader :name

  def initialize(name)
    @name = name
    @posts = []
  end

  def add_post(a_post)
    @posts << a_post
  end

  def all
    @posts # Dangerous... caller could erase all of our posts!
  end

  def empty?
    @posts.empty?
  end

  def [](index)
    @posts[index]
  end
end

