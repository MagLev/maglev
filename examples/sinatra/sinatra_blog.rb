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

RubyContext.load_context

require 'sinatra'
require 'post'
require 'blog'
require 'maglev_transaction_wrapper'

# Rack middleware to wrap http requests in a gemstone transaction.  Only
# data will be saved, not methods...
use MagLevTransactionWrapper

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

  def get_blog(name)
    $blogs.detect { |blog| blog.name == b_name }
    # TODO: Raise or redirect from here if can't find the blog?
  end
end

get '/' do
  blogs = all_blogs
  if blogs.empty?
    "There are no blogs<br />" + new_post_link
  else
    "<ul>" + posts.all.collect do |post|
      "<li>#{post[:title]} - #{post.body}</li>"
    end + "</ul><br />" + new_post_link
  end
end

post '/blogs/:name' do
  b_name = params[:name]
  # TODO: Move this to ActiveObject...
  cur_blog = all_blogs.detect { |blog| blog.name == b_name }
  raise "A blog named '#{b_name}' already exists" unless cur_blog.nil?
  cur_blog = Blog.new(:name)
  all_blogs << cur_blog
  redirect "/blogs/#{name}/"  # TODO: Make sure this is a get
end

get '/blogs/:name/' do
  name = params[:name]
  blog = get_blog(name)
  if blog.nil?
    "ERROR: Couldn't find #{blog}"  # TODO redirect
  else
    "<ul>" + posts.all.collect do |post|
      "<li>#{post[:title]} - #{post.body}</li>"
    end + "</ul><br />" + new_post_link
  end
end

# get '/:blog' do
#   blog = blogs.
# end
# get '/new' do
#   body do
#     <<-eos
#       <h3>Add a new post</h3>
#         <form action="/create" method="post">
#         <label for="title">Title</label><br />
#         <input type="text" name="title" /><br />
#         <label for="body">Body</label><br />
#         <textarea name="body"></textarea><br />
#         <input type="submit" name="submit" value="Add" />
#       </form>
#     eos
#   end
# end

# post '/create' do
#   title, body = params[:title], params[:body]
#   puts "==== /create   title: #{title}"

# #  posts.insert(:title => title, :body => body)
# #  posts.insert(params)
#   redirect '/'
# end

# get '/index' do
#   @title = "Erb Test"
#   @body = "It works..."
#   erb :index
# end
