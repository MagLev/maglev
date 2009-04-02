# A small blog example using sinatra
# This code a modified version of http://www.xnot.org/sinatra/beginning.html


# This will be a restful blog app, that hosts multiple blogs.  Each blog
# has mulltiple posts, and each post can have multiple comments.  That's
# about it.  All of the URLs will be of the form:
#
#   /                               # List the blogs / welcome page
#   /blog_name                      # Access main page of a blog
#   /blog_name/post_id              # Access a particular post of a blog
#   /blog_name/post_id/comment_id   # Access a comment on a post
#
# Some issues in running sinatra under MagLev
#
# 1: Development mode and reloading files
#    In development mode, Sinatra reloads the application with every request.
#    This means you can't
#
puts "== sinatra_blog.rb"
require 'setup.rb'

require 'sinatra'
require 'post'
require 'blog'

if running_maglev?
  # If we're running MagLev, then load some Rack middleware to wrap http
  # requests in a gemstone transaction.
  require 'txn_wrapper'
  use MagLevTransactionWrapper
end

configure(:development) do
  set :server, 'webrick'
  set :dump_errors, true
  set :raise_errors, true
  set :reload, false
  set :logging, false
  #set :port, 4567
end

helpers do
  def all_blogs
    if $blogs.nil?
      $blogs = Hash.new
      n = 'default blog'
      $blogs[n] = Blog.new(n)
    end
    $blogs
  end

  # Ensures that there is not already a blog named +name+, and then creates
  # the blog Raises an exception if the name is already used.  Returns the
  # new blog.
  def create_blog(name)
    blogs = all_blogs
    if blogs.has_key? name
      blog = nil
    else
      blog = Blog.new(name)
      blogs[name] = blog
    end
    blog
  end

  def get_blog(name)
    all_blogs[name]
  end

  def flash(msg)
    @flash << msg
  end

  def get_flash
    flash = @flash.to_s
    @flash = nil
    flash
  end
end

get '/' do
  @blogs = all_blogs
  erb :blogs
end

get '/blogs/:name' do
  name = params[:name]
  @blog = get_blog(name)
  if @blog.nil?
    # Or do we do a 404?
    flash "Can't find blog '#{name}'"
    redirect '/'
  else
    erb :index
  end
end

# Create a new blog
post '/blogs' do
  # TODO: check params for nil etc.
  # TODO: properly quote and otherwise validate name
  name = params[:name]
  if create_blog(name)
    redirect "/blogs/#{name}"
  else
    flash "Can't create blog named #{name}"
    redirect '/'
  end
end

get '/save/code' do
  puts "=== Saving code... ==="
  RubyContext.save_context
  Gemstone.commitTransaction
  redirect '/'
end

get '/reload/app' do
  puts "== reloading app code"
  load 'sinatra_blog.rb'
end
