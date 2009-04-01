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
SINATRA_DIR = File.dirname(__FILE__) + '/../../src/external/Sinatra/lib'
RACK_DIR    = File.dirname(__FILE__) + '/../../src/external/Rack/lib'

$:.unshift(SINATRA_DIR)
$:.unshift(RACK_DIR)
$:.unshift(File.dirname(__FILE__))

#RubyContext.load_context

require 'sinatra'
require 'post'
require 'blog'
#require 'txn_wrapper'

# Rack middleware to wrap http requests in a gemstone transaction.  Only
# data will be saved, not methods...
#use MagLevTransactionWrapper

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
    $blogs ||= { }
  end

  # Ensures that there is not already a blog named +name+, and then creates
  # the blog Raises an exception if the name is already used.  Returns the
  # new blog.
  #
  # TODO: What is the Sinatra way of redirecting etc.?
  def create_blog(name)
    blog = $blogs[name]
    raise "A blog named '#{name}' already exists" unless blog.nil?
    blog = Blog.new(name)
    $blogs[name] = blog
    blog
  end

  def get_blog(name)
    $blogs ||= { }
    $blogs[name]
    # TODO: Raise or redirect from here if can't find the blog?
  end
end

get '/' do
  puts "============= get '/'"
  @blogs = all_blogs
  erb :blogs
end

get '/blogs/:name' do
  puts "============= get '/blogs/:name'"
  name = params[:name]
  @blog = get_blog(name)
  raise "Can't find blog '#{name}'" if @blog.nil?
  erb :index
end

# Create a new blog
post '/blogs' do
  puts "============= post '/blogs'"
  # TODO: check params for nil etc.
  # TODO: properly quote and otherwise validate name
  name = params[:name]
  create_blog(name)
  redirect "/blogs/#{name}"
end
