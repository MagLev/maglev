puts "=== READING blog.rb"
class Blog
  attr_reader :name

  def initialize(name)
    @name = name
    @posts = Hash.new
  end

  def add_post(a_post)
    @posts[a_post.title] = a_post
  end
  def empty?
    @posts.empty?
  end
  def each(&block)
    @posts.each(&block)
  end
end

