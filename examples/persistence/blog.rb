# A simple blog with comments for experimentation.  The names of the
# classes, "Post" and "Comment", as well as the list of attributes and
# their names, comes from the blog example from the Rails 2.2.2
# getting_started_with_rails.txt file.  See the TODO.txt for hints of rails
# features to add to this example.
#

class Post
  # These are the attributes and names from the ra
  attr_reader :name, :title, :content

  def initialize(name, title, content, timestamp=Time.now)
    @name, @title, @content, @timestamp = name, title, content, timestamp
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
    "Post #{@name} #{@title} (#{@timestamp}) #{@content}: \n#{@comments.join("\n")}"
  end
end


class UseCases
  def post_index
    # @posts = Post.find(:all)
#     respond_to do |format|
#       format.html # index.html.erb
#       format.xml  { render :xml => @posts }
#     end
  end

  def post_new
#    @post = Post.new
  end

  def post_create
#     @post = Post.new(params[:post])

#     respond_to do |format|
#       if @post.save
#         flash[:notice] = 'Post was successfully created.'
#         format.html { redirect_to(@post) }
#         format.xml  { render :xml => @post, :status => :created, :location => @post }
#       else
#         format.html { render :action => "new" }
#         format.xml  { render :xml => @post.errors, :status => :unprocessable_entity }
#       end
#     end
  end

  def post_show
#     @post = Post.find(params[:id])

#     respond_to do |format|
#       format.html # show.html.erb
#       format.xml  { render :xml => @post }
#     end
  end

  def post_update
#     @post = Post.find(params[:id])

#     respond_to do |format|
#       if @post.update_attributes(params[:post])
#         flash[:notice] = 'Post was successfully updated.'
#         format.html { redirect_to(@post) }
#         format.xml  { head :ok }
#       else
#         format.html { render :action => "edit" }
#         format.xml  { render :xml => @post.errors, :status => :unprocessable_entity }
#       end
#     end
  end

  def post_destroy
#     @post = Post.find(params[:id])
#     @post.destroy

#     respond_to do |format|
#       format.html { redirect_to(posts_url) }
#       format.xml  { head :ok }
#     end
  end
end


$all_blogs = { }
Gemstone.commitTransaction


my_name 'Ogdred Weary'
$all_blogs[my_name] = Blog.new("My Spiffy Blog", my_name)



#  No real purpose....?
# class Blog
#   attr_reader :author, :title, :entries
#   def initialize(author, title)
#     @author = author
#     @title = title
#     @entries = []
#   end

#   def to_s
#     "BLOG: #{@title} by #{@author}: #{@entries.join("\n\n")}"
#   end
# end

