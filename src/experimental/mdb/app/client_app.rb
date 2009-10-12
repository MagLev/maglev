require 'rubygems'
require 'sinatra'

require 'mdb/client'
require 'blog'

# The BlogApp implements the View and Controller parts of the MVC pattern.
# This app receives end user http requests, invokes a model method (through
# a "view"), and then renders the appropriate view from the model data.
#
# Run by MRI
# Listens on port 3333
# Contacts MDB on port 4567
#
=begin
    |------+------------+-----------------------+----------|
    | Verb | Route      | Action                | View     |
    |------+------------+-----------------------+----------|
    | GET  | /          | redirect              | N/A      |
    | GET  | /posts     | List recent posts     | :index   |
    | GET  | /posts/new | Show new post form    | :newpost |
    | POST | /posts     | Create post from data | :show    |
    | GET  | /posts/:id | Show post with id     | :show    |
    | PUT  | /posts/:id | Update post from data | :show    |
    |------+------------+-----------------------+----------|
=end

class BlogApp < Sinatra::Base
  set :app_file, File.dirname(__FILE__)
  set :static, true

  # TODO: These need to be shared between the migrations and the app.  put
  # in a config file?
  SERVER  = 'http://localhost:4567'
  POSTS_DB = 'theBlogPosts'
  TAGS_DB  = 'theBlogTags'

  def initialize(*args)
    super
    @server = MDB::RESTServer.new SERVER
    @posts_db = @server[POSTS_DB]
    @tags_db  = @server[TAGS_DB]
    puts "-- @posts_db: #{@posts_db}"
    puts "-- @tags_db:  #{@tags_db}"
    @title = "MRI Blog Using MagLevDB"
    @nav_bar =  <<-EOS
        <ul class="menu">
          <li><a href="/">Home</a></li>
          <li><a href="/posts/new">New Post</a></li>
        </ul>
    EOS
  end

  get '/' do
    redirect '/posts'
  end

  get '/posts' do
    @posts = @posts_db.execute_view(:recent)
    puts "-- /posts: @posts: #{@posts.inspect}"
    erb :home
  end

  # Display a form to create a new blog post
  #
  # This route must go before /posts/:id, otherwise /posts/new matches
  # /posts/:id and we try to find a db named 'new'
  get '/posts/new' do
    erb :newpost
  end

  get '/posts/:id' do
    @post = @posts_db.get(params[:id])
    erb :post
  end

  # Create a new blog post
  # Submitting a /posts/new form goes here.
  # On success, redirects to show
  post '/posts' do
    tags = get_tags params
    new_params = {
      :title => params[:title],
      :text => params[:text],
      :tags => tags
    }
    post = Post.new(new_params)
    id = @posts_db.add(post)
    redirect "/posts/#{id}"
  end

  def get_tags(params)
    puts "TAGS: #{params[:tags].inspect}"
    tags = params[:tags].split
    tags.map { |t| Tag.new(t) }
  end
end

# BlogApp.run!   :host  => 'localhost', :port => 3333
