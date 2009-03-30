puts "=== READING post.rb"
class Post
  attr_reader :title, :content, :date

  def initialize(title, content, date=Time.now)
    @title, @content, @date = title, content, date
    @comments = []
  end

  def add_comment(comment)
    @comments << comment
    # Could also make a bi-directional link:
    #   comment.set_entry(self)
    # This is fodder for the associationn management gizmo yet to be
    # developed.
  end

  def to_s
    "Post #{@title} (#{@date}) #{@content}: \n#{@comments.join("\n")}"
  end
end
