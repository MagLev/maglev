# A small blog example using sinatra
# This code a modified version of http://www.xnot.org/sinatra/beginning.html

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

require 'sinatra'
require 'post'
require 'blog'
require 'maglev_transaction_wrapper'

# Rack middleware to wrap http requests in a gemstone transaction.  Only
# data will be saved, not methods...
use MagLevTransactionWrapper

configure(:development) do
  set :dump_errors, true
  set :raise_errors, true
  set :logging, false
  #set :port, 4567
end

helpers do
  # Return an array of all the posts
  def posts
    $my_blog ||= Blog.new("My Blog")
    $my_blog.all
  end

  def new_post_link
    "<a href='/new'>Add</a> a new post"
  end
end

get '/' do
  body do
    if posts.empty?
      "There are no posts<br />" + new_post_link
    else
      "<ul>" + posts.all.collect do |post|
        "<li>#{post[:title]} - #{post.body}</li>"
      end + "</ul><br />" + new_post_link
    end
  end
end

get '/new' do
  body do
    <<-eos
      <h3>Add a new post</h3>
        <form action="/create" method="post">
        <label for="title">Title</label><br />
        <input type="text" name="title" /><br />
        <label for="body">Body</label><br />
        <textarea name="body"></textarea><br />
        <input type="submit" name="submit" value="Add" />
      </form>
    eos
  end
end

post '/create' do
  puts "==== /create"

  title, body = params[:title], params[:body]
#  posts.insert(:title => title, :body => body)
#  posts.insert(params)
  redirect '/'
end
