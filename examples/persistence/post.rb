class Post
  # These are the attributes and names from the ra
  attr_reader :name, :title, :content

  @@all_posts = []
  def self.add_post(a_post)
    puts "=== Post.add_post(#{a_post.to_s})"
    puts "=== Post.add_post: all_posts: #{@@all_posts.inspect}"
    @@all_posts << a_post
  end

  def self.all
    puts "=== Post.all: all_posts:  #{@@all_posts.inspect}"
    @@all_posts # Dangerous... caller could erase all of our posts!
  end

  def self.empty?
    puts "=== Post.empty?: #{@@all_posts.empty?} (#{@@all_posts.inspect})"
    @@all_posts.empty?
  end

  def self.insert(params)
    puts "=== Post.insert(#{params.inspect})"
    the_new_post = new("a name", params[:title], params[:body])
    add_post(the_new_post)
    the_new_post
  end

  def self.[](index)
    @@all_posts[index]
  end

  def initialize(name, title, content, timestamp=Time.now)
    @name, @title, @content, @timestamp = name, title, content, timestamp
    @comments = []
    Post.add_post(self)
  end

  def add_comment(comment)
    @comments << comment
    # Could also make a bi-directional link:
    #   comment.set_entry(self)
    # This is fodder for the associationn management gizmo yet to be
    # developed.
  end

  def to_s
    "Post #{@name} #{@title} (#{@timestamp}) #{@content}: \n#{@comments.join("\n")}"
  end
end

if __FILE__ == $0
  puts "Creating posts..."
  10.times do |i|
    Post.insert(:title => "Post-#{i}", :body => "This is the body for post #{i}")
  end
  p Post.empty?
end
