puts "=== READING blog.rb"
class Blog
  attr_reader :name, :posts

  def initialize(name)
    @name = name
    @posts = []
  end

  def add_post(a_post)
    @posts << a_post
  end

  def empty?
    @posts.empty?
  end

  def [](index)
    @posts[index]
  end
end

